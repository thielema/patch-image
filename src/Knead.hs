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
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr)

import qualified Distribution.Simple.Utils as CmdLine
import qualified Distribution.Verbosity as Verbosity
import Distribution.Verbosity (Verbosity)
import Text.Printf (printf)

import Control.Arrow (arr)
import Data.Foldable (forM_)
import Data.Tuple.HT (mapTriple, fst3, swap)
import Data.Int (Int64)
import Data.Word (Word8, Word32)



type Size = Int64
type Channel sh = Phys.Array (sh, (Size, Size))
type Plane = Phys.Array (Size, Size)
type SymbChannel sh = Symb.Array (sh, (Size, Size))
type SymbPlane = Symb.Array (Size, Size)
type ColorImage a = Phys.Array (Size, Size) (YUV a)
type ColorImage8 = ColorImage Word8

type YUV a = (a,a,a)


readImage :: Verbosity -> FilePath -> IO ColorImage8
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
                     (fromIntegral $ Pic.imageHeight pic,
                      fromIntegral $ Pic.imageWidth pic)
                     (castForeignPtr $ fst $ SV.unsafeToForeignPtr0 dat)
            _ -> ioError $ userError "unsupported image type"


vectorStorableFrom ::
   (Shape.C sh, SV.Storable a) =>
   (ForeignPtr c -> ForeignPtr a) ->
   Phys.Array sh c -> SV.Vector a
vectorStorableFrom castArray img =
   SV.unsafeFromForeignPtr0
      (castArray $ Phys.buffer img) (fromIntegral $ Shape.size $ Phys.shape img)

imageFromArray ::
   (Pic.PixelBaseComponent c ~ a, SV.Storable a) =>
   (ForeignPtr b -> ForeignPtr a) -> Phys.Array (Size,Size) b -> Pic.Image c
imageFromArray castArray img =
   let (height, width) = Phys.shape img
   in Pic.Image {
         Pic.imageWidth = fromIntegral width,
         Pic.imageHeight = fromIntegral height,
         Pic.imageData = vectorStorableFrom castArray img
      }

writeImage :: Int -> FilePath -> ColorImage8 -> IO ()
writeImage quality path img =
   Pic.saveJpgImage quality path $ Pic.ImageYCbCr8 $
      imageFromArray castForeignPtr img

writeGrey :: Int -> FilePath -> Plane Word8 -> IO ()
writeGrey quality path img =
   Pic.saveJpgImage quality path $ Pic.ImageY8 $ imageFromArray id img


fromInt ::
   (MultiValue.NativeInteger i ir, MultiValue.NativeFloating a ar) =>
   Exp i -> Exp a
fromInt = Expr.liftM MultiValue.fromIntegral

floatFromByte ::
   (MultiValue.NativeFloating a ar,
    MultiValue.PseudoRing a, MultiValue.Real a,
    MultiValue.RationalConstant a) =>
   Exp Word8 -> Exp a
floatFromByte = (* Expr.fromRational' (recip 255)) . fromInt

byteFromFloat ::
   (MultiValue.NativeFloating a ar,
    MultiValue.Field a, MultiValue.Real a,
    MultiValue.RationalConstant a) =>
   Exp a -> Exp Word8
byteFromFloat = fastRound . (255*) . Expr.max 0 . Expr.min 1


imageFloatFromByte ::
   (Symb.C array, Shape.C sh,
    MultiValue.NativeFloating a ar,
    MultiValue.PseudoRing a, MultiValue.Real a,
    MultiValue.RationalConstant a) =>
   array sh Word8 -> array sh a
imageFloatFromByte = Symb.map floatFromByte

imageByteFromFloat ::
   (Symb.C array, Shape.C sh,
    MultiValue.NativeFloating a ar,
    MultiValue.Field a, MultiValue.Real a,
    MultiValue.RationalConstant a) =>
   array sh a -> array sh Word8
imageByteFromFloat = Symb.map byteFromFloat


colorImageFloatFromByte ::
   (Symb.C array, Shape.C sh,
    MultiValue.NativeFloating a ar,
    MultiValue.PseudoRing a, MultiValue.Real a,
    MultiValue.RationalConstant a) =>
   array sh (YUV Word8) -> array sh (YUV a)
colorImageFloatFromByte =
   Symb.map $ Expr.modify (atom,atom,atom) $
      mapTriple (floatFromByte, floatFromByte, floatFromByte)

colorImageByteFromFloat ::
   (Symb.C array, Shape.C sh,
    MultiValue.NativeFloating a ar,
    MultiValue.Field a, MultiValue.Real a,
    MultiValue.RationalConstant a) =>
   array sh (YUV a) -> array sh (YUV Word8)
colorImageByteFromFloat =
   Symb.map $ Expr.modify (atom,atom,atom) $
      mapTriple (byteFromFloat, byteFromFloat, byteFromFloat)


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
   Exp (Size,Size) ->
   array sh (Size,Size) ->
   array sh (Size,Size)
limitIndices sh =
   Symb.map
      (case Expr.unzip sh of
         (height, width) ->
            Expr.modify (atom, atom) $
               \(y,x) ->
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


type VecExp a v = Arith.Vec (Exp a) (Exp v)

vecYUV :: (MultiValue.PseudoRing a) => VecExp a (YUV a)
vecYUV =
   Arith.Vec {
      Arith.vecAdd =
         Expr.modify2 (atom,atom,atom) (atom,atom,atom) $
            \(ay,au,av) (by,bu,bv) ->
               (Expr.add ay by, Expr.add au bu, Expr.add av bv),
      Arith.vecScale =
         Expr.modify2 atom (atom,atom,atom) $
            \a (by,bu,bv) -> (Expr.mul a by, Expr.mul a bu, Expr.mul a bv)
   }

gatherFrac ::
   (MultiValue.NativeFloating a ar,
    MultiValue.Real a, MultiValue.Field a,
    MultiValue.RationalConstant a,
    MultiValue.C v) =>
   VecExp a v ->
   SymbPlane v ->
   SymbPlane (a,a) ->
   SymbPlane v
gatherFrac vec src poss =
   let possSplit =
         Symb.map
            (Expr.modify (atom, atom) $ \(y,x) ->
               let (xi,xf) = splitFraction x
                   (yi,yf) = splitFraction y
               in  ((yf,xf), (yi,xi)))
            poss
       possFrac = Symb.map Expr.fst possSplit
       possInt = Symb.map Expr.snd possSplit
       gather = flip Symb.gather src . limitIndices (Symb.shape src)
       interpolateHoriz possIntShifted =
         Symb.zipWith (Arith.cubicIpVec vec . Expr.unzip4)
            (Symb.zip4
               (gather $ shiftIndicesHoriz (-1) possIntShifted)
               (gather $ shiftIndicesHoriz   0  possIntShifted)
               (gather $ shiftIndicesHoriz   1  possIntShifted)
               (gather $ shiftIndicesHoriz   2  possIntShifted))
            (Symb.map Expr.snd possFrac)

   in  Symb.zipWith (Arith.cubicIpVec vec . Expr.unzip4)
         (Symb.zip4
            (interpolateHoriz $ shiftIndicesVert (-1) possInt)
            (interpolateHoriz $ shiftIndicesVert   0  possInt)
            (interpolateHoriz $ shiftIndicesVert   1  possInt)
            (interpolateHoriz $ shiftIndicesVert   2  possInt))
         (Symb.map Expr.fst possFrac)


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
   (SV.Storable a, MultiMem.C a,
    MultiValue.Real a, MultiValue.Field a,
    MultiValue.RationalConstant a, MultiValue.NativeFloating a ar,
    MultiValue.C v) =>
   VecExp a v ->
   Exp (a, a) ->
   Exp (a, a) ->
   Exp (Size, Size) ->
   SymbPlane v ->
   SymbPlane (MaskBool, v)
rotateStretchMove vec rot mov sh img =
   let coords = rotateStretchMoveCoords rot mov sh
       (heightSrc, widthSrc) = Expr.unzip $ Symb.shape img
   in  Symb.zip
         (validCoords (widthSrc, heightSrc) coords)
         (gatherFrac vec img coords)

rotate ::
   (SV.Storable a, MultiMem.C a,
    MultiValue.Real a, MultiValue.Field a,
    MultiValue.RationalConstant a, MultiValue.NativeFloating a ar,
    MultiValue.C v) =>
   VecExp a v ->
   Exp (a, a) ->
   SymbPlane v ->
   SymbPlane v
rotate vec rot img =
   let (height, width) = Expr.unzip $ Symb.shape img
       ((left, right), (top, bottom)) =
         Arith.boundingBoxOfRotatedGen (Expr.min, Expr.max)
            (Expr.unzip rot) (fromInt width, fromInt height)
   in  Symb.map Expr.snd $
       rotateStretchMove vec rot (Expr.zip (-left) (-top))
         (Expr.zip (ceilingToInt (bottom-top)) (ceilingToInt (right-left)))
         img


runRotate :: IO (Float -> ColorImage8 -> IO ColorImage8)
runRotate = do
   rot <-
      PhysP.render $
      SymbP.withExp
         (\rot ->
            colorImageByteFromFloat . rotate vecYUV rot .
            colorImageFloatFromByte)
         (arr fst) (PhysP.feed $ arr snd)
   return $ \ angle img -> rot ((cos angle, sin angle), img)


brightnessPlane ::
   (Symb.C array, Shape.C size) =>
   array size (YUV a) -> array size a
brightnessPlane = Symb.map $ Expr.modify (atom,atom,atom) fst3

rowHistogram ::
   (Symb.C array, Shape.C size, MultiValue.Additive a) =>
   array (size, Size) (YUV a) -> array size a
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


runScoreRotation :: IO (Float -> ColorImage8 -> IO Float)
runScoreRotation = do
   rot <-
      PhysP.render $
      SymbP.withExp
         (\rot ->
            rowHistogram . rotate vecYUV rot . colorImageFloatFromByte)
         (arr fst) (PhysP.feed $ arr snd)
   score <- PhysP.the $ scoreHistogram (PhysP.feed $ arr id)
   return $ \ angle img -> score =<< rot ((cos angle, sin angle), img)



emptyCanvas :: IO ((Size, Size) -> IO (Plane (Word32, YUV Float)))
emptyCanvas = PhysP.render $ SymbP.fill (arr id) $ return (0, (0,0,0))


type MaskBool = Word8

maskFromBool :: Exp Bool -> Exp MaskBool
maskFromBool = Expr.liftM $ MultiValue.liftM $ LLVM.zext

intFromBool :: Exp MaskBool -> Exp Word32
intFromBool = Expr.liftM $ MultiValue.liftM $ LLVM.ext

addToCanvas ::
   (MultiValue.PseudoRing a, MultiValue.NativeFloating a ar) =>
   VecExp a v ->
   SymbPlane (MaskBool, v) ->
   SymbPlane (Word32, v) ->
   SymbPlane (Word32, v)
addToCanvas vec =
   Symb.zipWith
      (Expr.modify2 (atom,atom) (atom,atom) $ \(mask, pic) (count, canvas) ->
         (Expr.add (intFromBool mask) count,
          Arith.vecAdd vec canvas $
          Arith.vecScale vec (fromInt $ intFromBool mask) pic))

updateCanvas ::
   IO ((Float,Float) -> (Float,Float) -> ColorImage8 ->
       Plane (Word32, YUV Float) ->
       IO (Plane (Word32, YUV Float)))
updateCanvas = do
   update <-
      PhysP.render $
      SymbP.withExp2
         (\rotMov pic countCanvas ->
            let (rot,mov) = Expr.unzip rotMov
            in  addToCanvas vecYUV
                  (rotateStretchMove vecYUV rot mov (Symb.shape countCanvas) $
                   colorImageFloatFromByte pic)
                  countCanvas)
         (arr fst)
         (PhysP.feed $ arr (fst.snd))
         (PhysP.feed $ arr (snd.snd))

   return $ \rot mov pic countCanvas ->
      update ((rot,mov), (pic, countCanvas))


finalizeCanvas :: IO ((Plane (Word32, YUV Float)) -> IO ColorImage8)
finalizeCanvas =
   PhysP.render $
      colorImageByteFromFloat $
      Symb.map
         (Expr.modify (atom,atom) $ \(count, pixel) ->
            Arith.vecScale vecYUV (recip $ fromInt count) pixel)
         (PhysP.feed (arr id))



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
