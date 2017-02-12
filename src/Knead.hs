{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified Arithmetic as Arith

import qualified Data.Array.Knead.Parameterized.Physical as PhysP
import qualified Data.Array.Knead.Parameterized.Symbolic as SymbP
import qualified Data.Array.Knead.Simple.Physical as Phys
import qualified Data.Array.Knead.Simple.ShapeDependent as ShapeDep
import qualified Data.Array.Knead.Simple.Symbolic as Symb
import qualified Data.Array.Knead.Index.Nested.Shape as Shape
import qualified Data.Array.Knead.Expression as Expr
import Data.Array.Knead.Expression (Exp)

import qualified LLVM.Extra.Multi.Value.Memory as MultiMem
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Core as LLVM
import LLVM.Extra.Multi.Value (atom)

import qualified Codec.Picture as Pic

import qualified Data.Vector.Storable as SV

import qualified Distribution.Simple.Utils as CmdLine
import qualified Distribution.Verbosity as Verbosity
import Distribution.Verbosity (Verbosity)
import Text.Printf (printf)

import Control.Arrow (arr)
import Control.Monad (liftM2)
import Data.Foldable (forM_)
import Data.Tuple.HT (swap)
import Data.Int (Int64)
import Data.Word (Word8, Word32)



type Size = Int64
type Channel sh = Phys.Array (sh, (Size, Size))
type Plane = Phys.Array (Size, Size)
type SymbChannel sh = Symb.Array (sh, (Size, Size))
type SymbPlane = Symb.Array (Size, Size)
type ColorImage = Phys.Array ((Size, Size), Size) Word8


readImage :: Verbosity -> FilePath -> IO ColorImage
readImage verbosity path = do
   epic <- Pic.readImage path
   case epic of
      Left msg -> ioError $ userError msg
      Right dynpic ->
         case dynpic of
            Pic.ImageYCbCr8 pic -> do
               let dat = Pic.imageData pic
               CmdLine.info verbosity $
                  printf "yuv %dx%d, size %d\n"
                     (Pic.imageWidth pic)
                     (Pic.imageHeight pic)
                     (SV.length dat)
               return $
                  Phys.Array
                     ((fromIntegral $ Pic.imageHeight pic,
                       fromIntegral $ Pic.imageWidth pic), 3)
                     (fst $ SV.unsafeToForeignPtr0 dat)
            _ -> ioError $ userError "unsupported image type"


vectorStorableFrom ::
   (Shape.C sh, SV.Storable a) => Phys.Array sh a -> SV.Vector a
vectorStorableFrom img =
   SV.unsafeFromForeignPtr0
      (Phys.buffer img) (fromIntegral $ Shape.size $ Phys.shape img)

imageFromArray ::
   (Shape.C sh, Pic.PixelBaseComponent a ~ c, SV.Storable c) =>
   (sh -> (Size, Size)) -> Phys.Array sh c -> Pic.Image a
imageFromArray getSize img =
   let (height, width) = getSize $ Phys.shape img
   in Pic.Image {
         Pic.imageWidth = fromIntegral width,
         Pic.imageHeight = fromIntegral height,
         Pic.imageData = vectorStorableFrom img
      }

writeImage :: Int -> FilePath -> ColorImage -> IO ()
writeImage quality path img =
   Pic.saveJpgImage quality path $ Pic.ImageYCbCr8 $ imageFromArray fst img

writeGrey :: Int -> FilePath -> Plane Word8 -> IO ()
writeGrey quality path img =
   Pic.saveJpgImage quality path $ Pic.ImageY8 $ imageFromArray id img


fromInt ::
   (MultiValue.NativeInteger i ir, MultiValue.NativeFloating a ar) =>
   Exp i -> Exp a
fromInt = Expr.liftM MultiValue.fromIntegral

imageFloatFromByte ::
   (Symb.C array, Shape.C sh,
    MultiValue.NativeFloating a ar,
    MultiValue.PseudoRing a, MultiValue.Real a,
    MultiValue.RationalConstant a) =>
   array sh Word8 -> array sh a
imageFloatFromByte =
   Symb.map ((* Expr.fromRational' (recip 255)) . fromInt)

imageByteFromFloat ::
   (Symb.C array, Shape.C sh,
    MultiValue.NativeFloating a ar,
    MultiValue.Field a, MultiValue.Real a,
    MultiValue.RationalConstant a) =>
   array sh a -> array sh Word8
imageByteFromFloat = Symb.map (fastRound . (255*) . Expr.max 0 . Expr.min 1)


cycleLeftDim3 :: Exp (Size, (Size,Size)) -> Exp ((Size,Size), Size)
cycleLeftDim3 = Expr.swap

cycleRightDim3 :: Exp ((Size,Size), Size) -> Exp (Size, (Size,Size))
cycleRightDim3 = Expr.swap

separateChannels ::
   (Symb.C array) =>
   array ((Size,Size), Size) a -> array (Size, (Size,Size)) a
separateChannels =
   ShapeDep.backpermute cycleRightDim3 cycleLeftDim3

interleaveChannels ::
   (Symb.C array) =>
   array (Size, (Size,Size)) a -> array ((Size,Size), Size) a
interleaveChannels =
   ShapeDep.backpermute cycleLeftDim3 cycleRightDim3


fastRound ::
   (MultiValue.NativeInteger i ir, MultiValue.NativeFloating a ar,
    MultiValue.Field a, MultiValue.Real a,
    MultiValue.RationalConstant a) =>
   Exp a -> Exp i
fastRound x = Expr.liftM MultiValue.truncateToInt $ x + 0.5 * signum x

-- ToDo: use floor instead of truncate
splitFraction ::
   (MultiValue.NativeFloating a ar,
    MultiValue.PseudoRing a, MultiValue.Real a,
    MultiValue.IntegerConstant a) =>
   Exp a -> (Exp Int64, Exp a)
splitFraction x =
   let i = Expr.liftM MultiValue.truncateToInt x
   in  (i, x - fromInt i)

-- ToDo: replace by real ceiling
ceilingToInt ::
   (MultiValue.NativeFloating a ar,
    MultiValue.PseudoRing a, MultiValue.Real a,
    MultiValue.IntegerConstant a) =>
   Exp a -> Exp Int64
ceilingToInt = Expr.liftM MultiValue.truncateToInt


limitIndices ::
   (Symb.C array, Shape.C sh) =>
   array () (Size,Size) ->
   array sh (Size,Size) ->
   array sh (Size,Size)
limitIndices =
   ShapeDep.backpermute2 (flip const) (const Expr.unit) id
      (Expr.modify2 (atom, atom) (atom, atom) $
         \(height, width) (y,x) ->
            let xc = Expr.max 0 $ Expr.min (width -1) x
                yc = Expr.max 0 $ Expr.min (height-1) y
            in  (yc, xc))

shiftIndicesHoriz, shiftIndicesVert ::
   (Symb.C array, Shape.C sh) =>
   Exp Size ->
   array sh (Size,Size) ->
   array sh (Size,Size)
shiftIndicesHoriz dx = Symb.map (Expr.mapSnd (dx+))
shiftIndicesVert  dy = Symb.map (Expr.mapFst (dy+))


replicateChannel ::
   (Symb.C array, Shape.C size, MultiValue.C sh, Shape.C sh) =>
   array (sh, size) b -> array size a -> array (sh, size) a
replicateChannel =
   flip $
   ShapeDep.backpermuteExtra
      (Expr.modify2 atom (atom, atom) $ \chanSh (sh, _) -> (sh, chanSh))
      Expr.snd


addChannelIndices ::
   (Symb.C array, Shape.C sh, Shape.Index sh ~ ix, Shape.C size) =>
   array (sh, size) b -> array size a -> array (sh, size) (ix, a)
addChannelIndices shape =
   Symb.mapWithIndex (\x a -> Expr.zip (Expr.fst x) a)
   .   
   replicateChannel shape

gatherFrac ::
   (Shape.C sh, MultiValue.C sh,
    MultiValue.NativeFloating a ar,
    MultiValue.Real a, MultiValue.Field a,
    MultiValue.RationalConstant a) =>
   SymbChannel sh a ->
   SymbPlane (a,a) ->
   SymbChannel sh a
gatherFrac src poss =
   let possSplit =
         Symb.map
            (Expr.modify (atom, atom) $ \(y,x) ->
               let (xi,xf) = splitFraction x
                   (yi,yf) = splitFraction y
               in  ((yf,xf), (yi,xi)))
            poss
       possFrac = Symb.map Expr.fst possSplit
       possInt = Symb.map Expr.snd possSplit
       gather =
         flip Symb.gather src . addChannelIndices src .
         limitIndices (Symb.map Expr.snd $ ShapeDep.shape src)
       interpolateHoriz possIntShifted =
         Symb.zipWith (Arith.cubicIp . Expr.unzip4)
            (Symb.zip4
               (gather $ shiftIndicesHoriz (-1) possIntShifted)
               (gather $ shiftIndicesHoriz   0  possIntShifted)
               (gather $ shiftIndicesHoriz   1  possIntShifted)
               (gather $ shiftIndicesHoriz   2  possIntShifted))
            (replicateChannel src $ Symb.map Expr.snd possFrac)

   in  Symb.zipWith (Arith.cubicIp . Expr.unzip4)
         (Symb.zip4
            (interpolateHoriz $ shiftIndicesVert (-1) possInt)
            (interpolateHoriz $ shiftIndicesVert   0  possInt)
            (interpolateHoriz $ shiftIndicesVert   1  possInt)
            (interpolateHoriz $ shiftIndicesVert   2  possInt))
         (replicateChannel src $ Symb.map Expr.fst possFrac)


rotateStretchMoveCoords ::
   (SV.Storable a, MultiMem.C a,
    MultiValue.Real a, MultiValue.Field a,
    MultiValue.RationalConstant a, MultiValue.NativeFloating a ar) =>
   Exp (a, a) ->
   Exp (a, a) ->
   Exp (Size, Size) ->
   SymbPlane (a, a)
rotateStretchMoveCoords rot mov =
   Symb.map
      (let trans =
            Arith.rotateStretchMoveBackPoint
               (Expr.unzip rot) (Expr.unzip mov)
       in  Expr.modify (atom,atom) $
               \(y,x) -> swap $ trans (fromInt x, fromInt y))
   .
   Symb.id

inRange :: (MultiValue.Comparison a) => Exp a -> Exp a -> Exp Bool
inRange =
   Expr.liftM2 $ \ size x -> do
      lower <- MultiValue.cmp LLVM.CmpLE MultiValue.zero x
      upper <- MultiValue.cmp LLVM.CmpLT x size
      MultiValue.and lower upper

inBox ::
   (MultiValue.Comparison a) =>
   (Exp a, Exp a) ->
   (Exp a, Exp a) ->
   Exp Bool
inBox (width,height) (x,y) =
   Expr.liftM2 MultiValue.and (inRange width x) (inRange height y)

validCoords ::
   (MultiValue.NativeFloating a ar,
    MultiValue.Field a, MultiValue.Real a,
    MultiValue.RationalConstant a) =>
   (Exp Size, Exp Size) ->
   SymbPlane (a, a) -> SymbPlane MaskBool
validCoords (width,height) =
   Symb.map $ Expr.modify (atom,atom) $ \(x,y) ->
      maskFromBool $ inBox (width,height) (fastRound x, fastRound y)

{- |
@rotateStretchMove rot mov@
first rotate and stretches the image according to 'rot'
and then moves the picture.
-}
rotateStretchMove ::
   (Shape.C sh,
    SV.Storable a, MultiMem.C a,
    MultiValue.Real a, MultiValue.Field a,
    MultiValue.RationalConstant a, MultiValue.NativeFloating a ar) =>
   Exp (a, a) ->
   Exp (a, a) ->
   Exp (Size, Size) ->
   SymbChannel sh a ->
   (SymbPlane MaskBool, SymbChannel sh a)
rotateStretchMove rot mov sh img =
   let coords = rotateStretchMoveCoords rot mov sh
       (heightSrc, widthSrc) =
         Expr.decompose (atom,atom) $ Expr.snd $ Symb.shape img
   in  (validCoords (widthSrc, heightSrc) coords, gatherFrac img coords)

rotate ::
   (Shape.C sh,
    SV.Storable a, MultiMem.C a,
    MultiValue.Real a, MultiValue.Field a,
    MultiValue.RationalConstant a, MultiValue.NativeFloating a ar) =>
   Exp (a, a) ->
   SymbChannel sh a ->
   SymbChannel sh a
rotate rot img =
   let (_chans, (height, width)) =
         Expr.decompose (atom, (atom,atom)) $ Symb.shape img
       ((left, right), (top, bottom)) =
         Arith.boundingBoxOfRotatedGen (Expr.min, Expr.max)
            (Expr.unzip rot) (fromInt width, fromInt height)
   in  snd $
       rotateStretchMove rot (Expr.zip (-left) (-top))
         (Expr.zip (ceilingToInt (bottom-top)) (ceilingToInt (right-left)))
         img


runRotate :: IO (Float -> ColorImage -> IO ColorImage)
runRotate = do
   rot <-
      PhysP.render $
      SymbP.withExp
         (\rot ->
            imageByteFromFloat . interleaveChannels . rotate rot .
            separateChannels . imageFloatFromByte)
         (arr fst) (PhysP.feed $ arr snd)
   return $ \ angle img -> rot ((cos angle, sin angle), img)


brightnessPlane ::
   (Symb.C array, Shape.C size) =>
   array (Size, size) a -> array size a
brightnessPlane = ShapeDep.backpermute Expr.snd (Expr.compose . (,) Expr.zero)

rowHistogram ::
   (Symb.C array, Shape.C size, MultiValue.Additive a) =>
   array (Size, (size, Size)) a -> array size a
rowHistogram = Symb.fold1 Expr.add . brightnessPlane


tailArr :: (Symb.C array) => array Size a -> array Size a
tailArr = ShapeDep.backpermute (Expr.max 0 . flip Expr.sub 1) (Expr.add 1)

differentiate ::
   (Symb.C array, MultiValue.Additive a) => array Size a -> array Size a
differentiate xs = Symb.zipWith Expr.sub (tailArr xs) xs

sqr :: MultiValue.PseudoRing a => Exp a -> Exp a
sqr = Expr.liftM $ \x -> MultiValue.mul x x

scoreHistogram ::
   (Symb.C array, MultiValue.PseudoRing a) => array Size a -> array () a
scoreHistogram = Symb.fold1All Expr.add . Symb.map sqr . differentiate


runScoreRotation :: IO (Float -> ColorImage -> IO Float)
runScoreRotation = do
   rot <-
      PhysP.render $
      SymbP.withExp
         (\rot ->
            rowHistogram . rotate rot . separateChannels . imageFloatFromByte)
         (arr fst) (PhysP.feed $ arr snd)
   score <- PhysP.the $ scoreHistogram (PhysP.feed $ arr id)
   return $ \ angle img -> score =<< rot ((cos angle, sin angle), img)



emptyCanvas ::
   (Shape.C sh, MultiMem.C sh, MultiMem.Struct sh ~ shs, LLVM.IsSized shs,
    SV.Storable sh) =>
   IO ((sh, (Size, Size)) -> IO (Plane Word32, Channel sh Float))
emptyCanvas = do
   emptyCounts <- PhysP.render $ SymbP.fill (arr id) 0
   emptyImage <- PhysP.render $ SymbP.fill (arr id) 0
   return $ \sh@(_depth, size) ->
      liftM2 (,) (emptyCounts size) (emptyImage sh)


type MaskBool = Word8

maskFromBool :: Exp Bool -> Exp MaskBool
maskFromBool = Expr.liftM $ MultiValue.liftM $ LLVM.zext

intFromBool :: Exp MaskBool -> Exp Word32
intFromBool = Expr.liftM $ MultiValue.liftM $ LLVM.ext

runAddToCanvas ::
   (Shape.C sh, MultiMem.C sh, MultiMem.Struct sh ~ shs, LLVM.IsSized shs,
    SV.Storable sh,
    MultiMem.C a, MultiValue.PseudoRing a, SV.Storable a,
    MultiValue.NativeFloating a ar) =>
   IO ((Plane MaskBool, Channel sh a) ->
       (Plane Word32, Channel sh a) ->
       IO (Plane Word32, Channel sh a))
runAddToCanvas = do
   addCounts <-
      PhysP.render $
      Symb.zipWith Expr.add
         (Symb.map intFromBool $ PhysP.feed (arr fst)) (PhysP.feed (arr snd))
   addCanvas <-
      PhysP.render $
      let mask = PhysP.feed (arr fst)
          pic = PhysP.feed (arr (fst.snd))
          canvas = PhysP.feed (arr (snd.snd))
      in  Symb.zipWith Expr.add canvas $ Symb.zipWith Expr.mul pic $
          replicateChannel pic (Symb.map (fromInt . intFromBool) mask)
   return $ \(mask, pic) (count, canvas) ->
      liftM2 (,)
         (addCounts (mask, count))
         (addCanvas (mask, (pic, canvas)))

addToCanvas ::
   (Shape.C sh, MultiValue.PseudoRing a, MultiValue.NativeFloating a ar) =>
   (SymbPlane MaskBool, SymbChannel sh a) ->
   (SymbPlane Word32, SymbChannel sh a) ->
   (SymbPlane Word32, SymbChannel sh a)
addToCanvas (mask, pic) (count, canvas) =
   (Symb.zipWith Expr.add (Symb.map intFromBool mask) count,
    Symb.zipWith Expr.add canvas $ Symb.zipWith Expr.mul pic $
      replicateChannel pic (Symb.map (fromInt . intFromBool) mask))

updateCanvas ::
   IO ((Float,Float) -> (Float,Float) -> ColorImage ->
       (Plane Word32, Channel Size Float) ->
       IO (Plane Word32, Channel Size Float))
updateCanvas = do
   let update select =
         PhysP.render $
         SymbP.withExp3
            (\rotMov pic count canvas ->
               let (rot,mov) = Expr.unzip rotMov
               in  select $
                   addToCanvas
                     (rotateStretchMove rot mov (Expr.snd $ Symb.shape canvas) $
                      separateChannels $ imageFloatFromByte pic)
                     (count,canvas))
            (arr fst) (PhysP.feed $ arr (fst.snd))
            (PhysP.feed $ arr (fst.snd.snd))
            (PhysP.feed $ arr (snd.snd.snd))

   updateCounts <- update fst
   updateImage <- update snd

   return $ \rot mov pic (count,canvas) ->
      liftM2 (,)
         (updateCounts ((rot,mov), (pic, (count,canvas))))
         (updateImage ((rot,mov), (pic, (count,canvas))))


finalizeCanvas ::
   IO ((Plane Word32, Channel Size Float) -> IO ColorImage)
finalizeCanvas =
   PhysP.render $
      let count = PhysP.feed (arr fst)
          canvas = PhysP.feed (arr snd)
      in  imageByteFromFloat $ interleaveChannels $
          ShapeDep.backpermute2 const id Expr.snd
             (/) canvas (Symb.map fromInt count)



rotateTest :: IO ()
rotateTest = do
   rot <- runRotate
   img <- readImage Verbosity.normal "/tmp/bild/artikel0005.jpeg"
   forM_ [0..11] $ \k -> do
      let path = printf "/tmp/rotated/%04d.jpeg" k
      putStrLn path
      writeImage 100 path =<< rot (fromInteger k * pi/6) img

scoreTest :: IO ()
scoreTest = do
   score <- runScoreRotation
   img <- readImage Verbosity.normal "/tmp/bild/artikel0005.jpeg"
   forM_ [-10..10] $ \k -> do
      print =<< score (fromInteger k * 2*pi/(360*10)) img

main :: IO ()
main = scoreTest
