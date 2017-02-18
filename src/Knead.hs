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
import Data.Array.Knead.Simple.Symbolic ((!))
import Data.Array.Knead.Expression (Exp)

import qualified LLVM.Extra.ScalarOrVector as SoV
import qualified LLVM.Extra.Arithmetic as LLVMArith
import qualified LLVM.Extra.Multi.Value.Memory as MultiMem
import qualified LLVM.Extra.Multi.Value as MultiValue
import LLVM.Extra.Multi.Value (atom)

import qualified LLVM.Core as LLVM
import qualified Codec.Picture as Pic

import qualified Data.Vector.Storable as SV
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr)

import qualified Distribution.Simple.Utils as CmdLine
import qualified Distribution.Verbosity as Verbosity
import Distribution.Verbosity (Verbosity)
import Text.Printf (printf)

import qualified Data.List as List
import Control.Arrow (arr)
import Control.Monad (foldM)
import Control.Applicative ((<$>))
import Data.Traversable (forM)
import Data.Foldable (forM_)
import Data.Ord.HT (comparing)
import Data.Tuple.HT (mapPair, mapTriple, mapThd3, fst3, snd3, thd3)
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


yuvByteFromFloat ::
   (MultiValue.NativeFloating a ar,
    MultiValue.Field a, MultiValue.Real a,
    MultiValue.RationalConstant a) =>
   Exp (YUV a) -> Exp (YUV Word8)
yuvByteFromFloat =
   Expr.modify (atom,atom,atom) $
      mapTriple (byteFromFloat, byteFromFloat, byteFromFloat)

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
colorImageByteFromFloat = Symb.map yuvByteFromFloat


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
               \(y,x) -> trans (fromInt x, fromInt y))
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
         (gatherFrac vec img $ Symb.map Expr.swap coords)

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


brightnessValue :: Exp (YUV a) -> Exp a
brightnessValue = Expr.modify (atom,atom,atom) fst3

brightnessPlane ::
   (Symb.C array, Shape.C size) =>
   array size (YUV a) -> array size a
brightnessPlane = Symb.map brightnessValue

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

findOptimalRotation :: IO ([Float] -> ColorImage8 -> IO Float)
findOptimalRotation = do
   scoreRotation <- runScoreRotation
   return $ \angles pic ->
      fmap (fst . List.maximumBy (comparing snd)) $
      forM angles $ \angle ->
         (,) angle <$> scoreRotation (angle * (pi/180)) pic



transpose :: SymbPlane a -> SymbPlane a
transpose = ShapeDep.backpermute Expr.swap Expr.swap

lowpassVert, lowpass ::
   (MultiValue.Field a, MultiValue.Real a, MultiValue.RationalConstant a) =>
   SymbPlane a -> SymbPlane a
lowpassVert img =
   let height = Expr.fst $ Symb.shape img
   in  generate (Symb.shape img) $ Expr.modify (atom,atom) $ \(y,x) ->
         Arith.smooth3
            (img ! (Expr.zip (Expr.max 0 (y-1)) x),
             img ! (Expr.zip y x),
             img ! (Expr.zip (Expr.min (height-1) (y+1)) x))

lowpass = transpose . lowpassVert . transpose . lowpassVert

nestM :: Monad m => Int -> (a -> m a) -> a -> m a
nestM n f x0 = foldM (\x () -> f x) x0 (replicate n ())

lowpassMulti :: IO (Int -> Plane Float -> IO (Plane Float))
lowpassMulti = do
   lp <- PhysP.render (Symb.lift1 lowpass $ PhysP.feed (arr id))
   return $ \n -> nestM n lp



emptyCanvas :: IO ((Size, Size) -> IO (Plane (Word32, YUV Float)))
emptyCanvas = PhysP.render $ SymbP.fill (arr id) $ return (0, (0,0,0))


type MaskBool = Word8

maskFromBool :: Exp Bool -> Exp MaskBool
maskFromBool = Expr.liftM $ MultiValue.liftM $ LLVM.zext

boolFromMask :: Exp MaskBool -> Exp Bool
boolFromMask =
   Expr.liftM $ MultiValue.liftM $
      LLVM.cmp LLVM.CmpNE (LLVM.zero :: LLVM.ConstValue MaskBool)

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


diffAbs :: (MultiValue.Real a) => Exp a -> Exp a -> Exp a
diffAbs = Expr.liftM2 $ \x y -> MultiValue.abs =<< MultiValue.sub x y

diffWithCanvas ::
   IO ((Float,Float) -> (Float,Float) -> ColorImage8 ->
       Plane (YUV Float) ->
       IO (Plane (MaskBool, Float)))
diffWithCanvas = do
   update <-
      PhysP.render $
      SymbP.withExp2
         (\rotMov pic avg ->
            let (rot,mov) = Expr.unzip rotMov
            in  Symb.zipWith
                  (Expr.modify2 (atom,atom) atom $ \(b,x) y ->
                     (b, diffAbs (brightnessValue x) (brightnessValue y)))
                  (rotateStretchMove vecYUV rot mov (Symb.shape avg) $
                   colorImageFloatFromByte pic)
                  avg)
         (arr fst)
         (PhysP.feed $ arr (fst.snd))
         (PhysP.feed $ arr (snd.snd))

   return $ \rot mov pic countCanvas ->
      update ((rot,mov), (pic, countCanvas))

finalizeCanvasFloat ::
   IO ((Plane (Word32, YUV Float)) -> IO (Plane (YUV Float)))
finalizeCanvasFloat =
   PhysP.render $
      Symb.map
         (Expr.modify (atom,atom) $ \(count, pixel) ->
            Arith.vecScale vecYUV (recip $ fromInt count) pixel)
         (PhysP.feed (arr id))

emptyPlainCanvas :: IO ((Size, Size) -> IO ColorImage8)
emptyPlainCanvas = PhysP.render $ SymbP.fill (arr id) $ return (0,0,0)

addMaskedToCanvas ::
   IO ((Float,Float) -> (Float,Float) -> ColorImage8 ->
       Plane MaskBool ->
       Plane (YUV Word8) ->
       IO (Plane (YUV Word8)))
addMaskedToCanvas = do
   update <-
      PhysP.render $
      SymbP.withExp3
         (\rotMov pic mask countCanvas ->
            let (rot,mov) = Expr.unzip rotMov
            in  Symb.zipWith3 Expr.ifThenElse
                  (Symb.map boolFromMask mask)
                  (Symb.map (yuvByteFromFloat . Expr.snd) $
                   rotateStretchMove vecYUV rot mov (Symb.shape countCanvas) $
                   colorImageFloatFromByte pic)
                  countCanvas)
         (arr fst)
         (PhysP.feed $ arr (fst3.snd))
         (PhysP.feed $ arr (snd3.snd))
         (PhysP.feed $ arr (thd3.snd))

   return $ \rot mov pic mask countCanvas ->
      update ((rot,mov), (pic, mask, countCanvas))


maybePlus ::
   (MultiValue.C a) =>
   (Exp a -> Exp a -> Exp a) ->
   Exp (Bool, a) -> Exp (Bool, a) -> Exp (Bool, a)
maybePlus f x y =
   let (xb,xv) = Expr.unzip x
       (yb,yv) = Expr.unzip y
   in  Expr.ifThenElse xb
         (Expr.compose (Expr.true, Expr.ifThenElse yb (f xv yv) xv)) y

maskedMinimum ::
   (Shape.C sh, Symb.C array, MultiValue.Real a) =>
   array (sh, Size) (Bool, a) -> array sh (Bool, a)
maskedMinimum = Symb.fold1 (maybePlus Expr.min)


generate ::
   (Shape.C sh) =>
   Exp sh -> (Exp (Shape.Index sh) -> Exp b) -> Symb.Array sh b
generate sh f = Symb.map f $ Symb.id sh

distanceMapBox ::
   (MultiValue.Field a, MultiValue.NativeFloating a ar,
    MultiValue.Real a, MultiValue.RationalConstant a) =>
   Exp (Size,Size) ->
   Exp ((a,a), (a,a), (Size,Size)) ->
   SymbPlane (Bool, (((a,(a,a)), (a,(a,a))), ((a,(a,a)), (a,(a,a)))))
distanceMapBox sh geom =
   let (rot, mov, extent@(width,height)) =
         Expr.decompose ((atom,atom),(atom,atom),(atom,atom)) geom
       widthf  = fromInt width
       heightf = fromInt height
       back  = Arith.rotateStretchMoveBackPoint rot mov
       forth = Arith.rotateStretchMovePoint rot mov
   in  generate sh $ Expr.modify (atom,atom) $ \(y,x) ->
         let (xsrc,ysrc) = back (fromInt x, fromInt y)
             leftDist = Expr.max 0 xsrc
             rightDist = Expr.max 0 $ widthf - xsrc
             topDist = Expr.max 0 ysrc
             bottomDist = Expr.max 0 $ heightf - ysrc
         in  (inBox extent (fastRound xsrc, fastRound ysrc),
              (((leftDist, forth (0,ysrc)),
                (rightDist, forth (widthf,ysrc))),
               ((topDist, forth (xsrc,0)),
                (bottomDist, forth (xsrc,heightf)))))

distance ::
   (MultiValue.Algebraic a, MultiValue.Real a,
    MultiValue.IntegerConstant a) =>
   Arith.Point2 (Exp a) -> Arith.Point2 (Exp a) -> Exp a
distance a b =
   Expr.liftM MultiValue.sqrt $ Arith.distanceSqr a b

outerProduct ::
   (Shape.C sha, Shape.C shb, Symb.C array) =>
   (Exp a -> Exp b -> Exp c) ->
   array sha a -> array shb b -> array (sha,shb) c
outerProduct =
   ShapeDep.backpermute2 Expr.zip Expr.fst Expr.snd

isZero ::
   (MultiValue.Comparison i, MultiValue.Integral i,
    MultiValue.IntegerConstant i) =>
   Exp i -> Exp Bool
isZero = Expr.liftM $ MultiValue.cmp LLVM.CmpEQ MultiValue.zero

expEven ::
   (MultiValue.Comparison i, MultiValue.Integral i,
    MultiValue.IntegerConstant i) =>
   Exp i -> Exp Bool
expEven = isZero . flip Expr.irem 2

separateDistanceMap ::
   (Symb.C array, Shape.C sh, MultiValue.C a) =>
   array sh (bool, ((a, a), (a, a))) ->
   array (sh, Size) (bool, a)
separateDistanceMap array =
   outerProduct
      (Expr.modify2 (atom, ((atom, atom), (atom, atom))) atom $
       \(b,(horiz,vert)) sel ->
          (b,
           Expr.ifThenElse (expEven $ Expr.idiv sel 2)
               (uncurry (Expr.ifThenElse (expEven sel)) horiz)
               (uncurry (Expr.ifThenElse (expEven sel)) vert)))
      array (Symb.lift0 $ Symb.id 4)


containedAnywhere ::
   (Symb.C array, Shape.C sh,
    MultiValue.Field a, MultiValue.NativeFloating a ar,
    MultiValue.Real a, MultiValue.RationalConstant a) =>
   array Size ((a,a), (a,a), (Size,Size)) ->
   array sh (a,a) ->
   array sh Bool
containedAnywhere geoms array =
   Symb.fold1 (Expr.liftM2 MultiValue.or) $
   outerProduct
      (Expr.modify2 (atom,atom) ((atom,atom),(atom,atom),(atom,atom)) $
       \(xdst,ydst) (rot, mov, extent) ->
         let (xsrc,ysrc) = Arith.rotateStretchMoveBackPoint rot mov (xdst,ydst)
         in  inBox extent (fastRound xsrc, fastRound ysrc))
      array geoms


distanceMapContained ::
   (MultiValue.RationalConstant a, MultiValue.NativeFloating a ar,
    MultiValue.PseudoRing a, MultiValue.Field a, MultiValue.Real a) =>
   Exp (Size, Size) ->
   Exp ((a, a), (a, a), (Size, Size)) ->
   Symb.Array Size ((a, a), (a, a), (Size, Size)) ->
   SymbPlane a
distanceMapContained sh this others =
   let distMap = separateDistanceMap $ distanceMapBox sh this
       contained =
          containedAnywhere others $
          Symb.map (Expr.snd . Expr.snd) distMap
   in  Symb.map (Expr.modify (atom,atom) $
                  \(valid, dist) -> Expr.ifThenElse valid dist 0) $
       maskedMinimum $
       Symb.zipWith
          (Expr.modify2 atom (atom,(atom,atom)) $ \c (b,(dist,_)) ->
             (Expr.liftM2 MultiValue.and c b, dist))
          contained distMap


pixelCoordinates ::
   (MultiValue.NativeFloating a ar) =>
   Exp (Size, Size) -> SymbPlane (a,a)
pixelCoordinates sh =
   generate sh $ Expr.modify (atom,atom) $ \(y,x) -> (fromInt x, fromInt y)

distanceMapPoints ::
   (Shape.C sh, Symb.C array,
    MultiValue.Real a, MultiValue.Algebraic a, MultiValue.IntegerConstant a) =>
   array sh (a,a) ->
   array Size (a,a) ->
   array sh a
distanceMapPoints a b =
   Symb.fold1 Expr.min $
   outerProduct (Expr.modify2 (atom,atom) (atom,atom) distance) a b


{- |
For every pixel
it computes the distance to the closest point on the image part boundary
which lies in any other image.
The rationale is that we want to fade an image out,
wherever is another image that can take over.
Such a closest point can either be a perpendicular point
at one of the image edges,
or it can be an image corner
or an intersection between this image border and another image border.
The first kind of points is computed by 'distanceMapContained'
and the second kind by 'distanceMapPoints'.
We simply compute the distances to all special points
and chose the minimal distance.
-}
distanceMap ::
   (MultiValue.Algebraic a, MultiValue.Real a,
    MultiValue.RationalConstant a,
    MultiValue.NativeFloating a ar) =>
   Exp (Size,Size) ->
   Exp ((a, a), (a, a), (Size,Size)) ->
   Symb.Array Size ((a, a), (a, a), (Size,Size)) ->
   Symb.Array Size (a, a) ->
   SymbPlane a
distanceMap sh this others points =
   Symb.zipWith Expr.min
      (distanceMapContained sh this others)
      (distanceMapPoints (pixelCoordinates sh) points)


pow ::
   (MultiValue.Repr LLVM.Value a ~ LLVM.Value ar,
    LLVM.IsFloating ar, SoV.TranscendentalConstant ar) =>
   Exp a -> Exp a -> Exp a
pow =
   flip $ Expr.liftM2 $ \(MultiValue.Cons x) (MultiValue.Cons y) ->
      fmap MultiValue.Cons $ LLVMArith.pow x y

distanceMapGamma ::
   (MultiValue.Algebraic a, MultiValue.Real a,
    MultiValue.RationalConstant a,
    MultiValue.NativeFloating a ar,
    SoV.TranscendentalConstant ar) =>
   Exp a ->
   Exp (Size,Size) ->
   Exp ((a, a), (a, a), (Size,Size)) ->
   Symb.Array Size ((a, a), (a, a), (Size,Size)) ->
   Symb.Array Size (a, a) ->
   SymbPlane a
distanceMapGamma gamma sh this others points =
   Symb.map (pow gamma) $ distanceMap sh this others points


emptyWeightedCanvas :: IO ((Size, Size) -> IO (Plane (Float, YUV Float)))
emptyWeightedCanvas = PhysP.render $ SymbP.fill (arr id) $ return (0, (0,0,0))

addToWeightedCanvas ::
   (MultiValue.PseudoRing a, MultiValue.NativeFloating a ar) =>
   VecExp a v ->
   SymbPlane (a, v) ->
   SymbPlane (a, v) ->
   SymbPlane (a, v)
addToWeightedCanvas vec =
   Symb.zipWith
      (Expr.modify2 (atom,atom) (atom,atom) $
         \(weight, pic) (weightSum, canvas) ->
            (Expr.add weight weightSum,
             Arith.vecAdd vec canvas $ Arith.vecScale vec weight pic))

geom64 ::
   ((Float,Float),(Float,Float),(Int,Int)) ->
   ((Float,Float),(Float,Float),(Size,Size))
geom64 = mapThd3 (mapPair (fromIntegral, fromIntegral))

updateWeightedCanvas ::
   IO (Float ->
       ((Float,Float),(Float,Float),(Int,Int)) ->
       [((Float,Float),(Float,Float),(Int,Int))] ->
       [Arith.Point2 Float] ->
       ColorImage8 ->
       Plane (Float, YUV Float) ->
       IO (Plane (Float, YUV Float)))
updateWeightedCanvas = do
   distances <-
      PhysP.render $
      SymbP.withExp2
         (\gammaShThis others points ->
            let (gamma, sh, this) = Expr.unzip3 gammaShThis
            in  distanceMapGamma gamma sh this others points)
         (arr fst)
         (PhysP.feed $ arr (fst.snd))
         (PhysP.feed $ arr (snd.snd))

   update <-
      PhysP.render $
      SymbP.withExp3
         (\this pic dist weightSumCanvas ->
            let (rot, mov, _) = Expr.unzip3 this
            in  addToWeightedCanvas vecYUV
                  (Symb.zip dist $
                   Symb.map Expr.snd $
                   rotateStretchMove vecYUV rot mov
                      (Symb.shape weightSumCanvas) $
                   colorImageFloatFromByte pic)
                  weightSumCanvas)
         (arr fst)
         (PhysP.feed $ arr (fst3.snd))
         (PhysP.feed $ arr (snd3.snd))
         (PhysP.feed $ arr (thd3.snd))

   return $ \gamma this others points pic weightSumCanvas -> do
      othersVec <- Phys.vectorFromList $ map geom64 others
      pointsVec <- Phys.vectorFromList points
      dists <-
         distances
            ((gamma, Phys.shape weightSumCanvas, geom64 this),
             (othersVec, pointsVec))
      update (geom64 this, (pic, dists, weightSumCanvas))


finalizeWeightedCanvas :: IO ((Plane (Float, YUV Float)) -> IO ColorImage8)
finalizeWeightedCanvas =
   PhysP.render $
      colorImageByteFromFloat $
      Symb.map
         (Expr.modify (atom,atom) $ \(weightSum, pixel) ->
            Arith.vecScale vecYUV (recip weightSum) pixel)
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
