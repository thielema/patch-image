{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified Option
import qualified State

import qualified MatchImageBorders
import qualified Arithmetic as Arith
import qualified Knead.CArray as KneadCArray
import qualified Complex as Komplex
import qualified Degree
import MatchImageBorders (arrayCFromKnead, arrayKneadFromC)
import Arithmetic (guardedPairs, maximum0)
import LinearAlgebra (
   absolutePositionsFromPairDisplacements, fixAtLeastOnePosition,
   layoutFromPairDisplacements, fixAtLeastOneAnglePosition,
   )
import Knead.Shape
         (Size, Vec2(Vec2), Dim1, Dim2, Shape2, Index2, Ix2,
          verticalVal, horizontalVal)
import Degree (Degree(Degree), getDegree)

import qualified Math.FFT as FFT

import qualified Data.Array.Knead.Parameterized.Render as RenderP
import qualified Data.Array.Knead.Simple.Physical as Phys
import qualified Data.Array.Knead.Simple.ShapeDependent as ShapeDep
import qualified Data.Array.Knead.Simple.Symbolic as Symb
import qualified Data.Array.Knead.Index.Nested.Shape as Shape
import qualified Data.Array.Knead.Expression as Expr
import Data.Array.Knead.Simple.Symbolic ((!))
import Data.Array.Knead.Expression
         (Exp, (==*), (/=*), (<*), (<=*), (>=*), (&&*))

import qualified Data.Array.CArray as CArray
import Data.Array.IArray (amap)
import Data.Array.CArray (CArray)
import Data.Array.MArray (thaw)

import qualified LLVM.Extra.ScalarOrVector as SoV
import qualified LLVM.Extra.Arithmetic as LLVMArith
import qualified LLVM.Extra.Multi.Value.Memory as MultiMem
import qualified LLVM.Extra.Multi.Value as MultiValue
import LLVM.Extra.Multi.Value (Atom, atom)

import qualified LLVM.Core as LLVM

import qualified Data.Complex as Complex
import Data.Complex (Complex)

import qualified Codec.Picture as Pic

import qualified Data.Vector.Storable as SV
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr)

import qualified System.FilePath as FilePath
import qualified System.IO as IO

import qualified Distribution.Simple.Utils as CmdLine
import qualified Distribution.Verbosity as Verbosity
import Distribution.Verbosity (Verbosity)
import Text.Printf (printf)

import qualified Control.Monad.HT as MonadHT
import Control.Monad (liftM2, when, join, foldM, (<=<))
import Control.Applicative (pure, (<$>), (<*>))

import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Function.HT (Id)
import Data.Monoid ((<>))
import Data.Maybe.HT (toMaybe)
import Data.Maybe (mapMaybe, isJust, isNothing)
import Data.Traversable (forM)
import Data.Foldable (forM_)
import Data.Ord.HT (comparing)
import Data.Tuple.HT
         (mapPair, mapFst, mapSnd, mapTriple, swap, mapThd3, fst3, uncurry3)
import Data.Word (Word8, Word32)



type SmallSize = Word32

type Plane = Phys.Array Dim2
type SymbPlane = Symb.Array Dim2
type ColorImage a = Phys.Array Dim2 (YUV a)
type ColorImage8 = ColorImage Word8

type YUV a = (a,a,a)

shape2 :: (Integral i) => i -> i -> Dim2
shape2 height width = Vec2 (fromIntegral height) (fromIntegral width)


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
                     (shape2 (Pic.imageHeight pic) (Pic.imageWidth pic))
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
   (ForeignPtr b -> ForeignPtr a) -> Phys.Array Dim2 b -> Pic.Image c
imageFromArray castArray img =
   let Vec2 height width = Phys.shape img
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


colorImageExtent :: ColorImage8 -> (Size, Size)
colorImageExtent pic =
   case Phys.shape pic of Vec2 height width -> (width, height)


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
   (MultiValue.NativeInteger i ir, MultiValue.NativeFloating a ar) =>
   Exp a -> Exp i
fastRound = Expr.liftM MultiValue.roundToIntFast

splitFraction ::
   (MultiValue.NativeFloating a ar) =>
   Exp a -> (Exp Size, Exp a)
splitFraction = Expr.unzip . Expr.liftM MultiValue.splitFractionToInt

ceilingToInt ::
   (MultiValue.NativeFloating a ar) =>
   Exp a -> Exp Size
ceilingToInt = Expr.liftM MultiValue.ceilingToInt




atomDim2 :: Shape2 (Atom i)
atomDim2 = Vec2 atom atom

atomIx2 :: Index2 (Atom i)
atomIx2 = Vec2 atom atom

dim2 :: Exp i -> Exp i -> Exp (Shape2 i)
dim2 y x = Expr.compose (Vec2 y x)

ix2 :: Exp i -> Exp i -> Exp (Index2 i)
ix2 y x = Expr.compose (Vec2 y x)


fromSize2 ::
   (MultiValue.NativeFloating a ar) =>
   (Exp Size, Exp Size) -> (Exp a, Exp a)
fromSize2 (x,y) = (fromInt x, fromInt y)



indexLimit :: SymbPlane a -> Index2 (Exp Size) -> Exp a
indexLimit img (Vec2 y x) =
   let (Vec2 height width) = Expr.decompose atomDim2 $ Symb.shape img
       xc = Expr.max 0 $ Expr.min (width -1) x
       yc = Expr.max 0 $ Expr.min (height-1) y
   in  img ! ix2 yc xc

limitIndices ::
   (Symb.C array, Shape.C sh) =>
   Exp Dim2 -> array sh Ix2 -> array sh Ix2
limitIndices sh =
   Symb.map
      (case Expr.decompose atomDim2 sh of
         (Vec2 height width) ->
            Expr.modify atomIx2 $
               \(Vec2 y x) ->
                  let xc = Expr.max 0 $ Expr.min (width -1) x
                      yc = Expr.max 0 $ Expr.min (height-1) y
                  in  Vec2 yc xc)

shiftIndicesHoriz, shiftIndicesVert ::
   (Symb.C array, Shape.C sh) =>
   Exp Size -> array sh Ix2 -> array sh Ix2
shiftIndicesHoriz dx =
   Symb.map $ Expr.modify atomIx2 $ \(Vec2 y x) -> Vec2 y (x+dx)
shiftIndicesVert  dy =
   Symb.map $ Expr.modify atomIx2 $ \(Vec2 y x) -> Vec2 (y+dy) x


type VecExp a v = Arith.Vec (Exp a) (Exp v)

vecYUV :: (MultiValue.PseudoRing a) => VecExp a (YUV a)
vecYUV =
   Arith.Vec {
      Arith.vecZero = Expr.compose (Expr.zero, Expr.zero, Expr.zero),
      Arith.vecAdd =
         Expr.modify2 (atom,atom,atom) (atom,atom,atom) $
            \(ay,au,av) (by,bu,bv) ->
               (Expr.add ay by, Expr.add au bu, Expr.add av bv),
      Arith.vecScale =
         Expr.modify2 atom (atom,atom,atom) $
            \a (by,bu,bv) -> (Expr.mul a by, Expr.mul a bu, Expr.mul a bv)
   }

{-
Generated code becomes too big for LLVM here. We need sharing!
-}
indexFrac ::
   (MultiValue.NativeFloating a ar,
    MultiValue.Real a, MultiValue.Field a,
    MultiValue.RationalConstant a) =>
   VecExp a v -> SymbPlane v -> Index2 (Exp a) -> Exp v
indexFrac vec img (Vec2 y x) =
   let (xi,xf) = splitFraction x
       (yi,yf) = splitFraction y
       interpolRow yc =
          Arith.cubicIpVec vec
             (indexLimit img (Vec2 yc (xi-1)),
              indexLimit img (Vec2 yc (xi  )),
              indexLimit img (Vec2 yc (xi+1)),
              indexLimit img (Vec2 yc (xi+2)))
             xf
   in  Arith.cubicIpVec vec
          (interpolRow (yi-1),
           interpolRow  yi,
           interpolRow (yi+1),
           interpolRow (yi+2))
          yf

indexFrac1 ::
   (MultiValue.NativeFloating a ar,
    MultiValue.Real a, MultiValue.Field a,
    MultiValue.RationalConstant a) =>
   VecExp a v -> SymbPlane v -> Index2 (Exp a) -> Exp v
indexFrac1 vec img (Vec2 y x) =
   let (xi,xf) = splitFraction x
       (yi,yf) = splitFraction y
       interpolRow yc =
          Arith.linearIpVec vec
             (indexLimit img (Vec2 yc (xi-1)),
              indexLimit img (Vec2 yc (xi+1)))
             xf
   in  Arith.linearIpVec vec
          (interpolRow  yi,
           interpolRow (yi+1))
          yf

gatherFrac, gatherFrac_ ::
   (MultiValue.NativeFloating a ar,
    MultiValue.Real a, MultiValue.Field a,
    MultiValue.RationalConstant a,
    MultiValue.C v) =>
   VecExp a v ->
   SymbPlane v ->
   SymbPlane (Index2 a) ->
   SymbPlane v
gatherFrac_ vec src =
   Symb.map (indexFrac vec src . Expr.decompose atomIx2)

gatherFrac vec src poss =
   let possSplit =
         Symb.map
            (Expr.modify atomIx2 $ \(Vec2 y x) ->
               let (xi,xf) = splitFraction x
                   (yi,yf) = splitFraction y
               in  (Vec2 yf xf, Vec2 yi xi))
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
            (Symb.map horizontalVal possFrac)

   in  Symb.zipWith (Arith.cubicIpVec vec . Expr.unzip4)
         (Symb.zip4
            (interpolateHoriz $ shiftIndicesVert (-1) possInt)
            (interpolateHoriz $ shiftIndicesVert   0  possInt)
            (interpolateHoriz $ shiftIndicesVert   1  possInt)
            (interpolateHoriz $ shiftIndicesVert   2  possInt))
         (Symb.map verticalVal possFrac)


rotateStretchMoveCoords ::
   (SV.Storable a, MultiMem.C a,
    MultiValue.Real a, MultiValue.Field a,
    MultiValue.RationalConstant a, MultiValue.NativeFloating a ar) =>
   Exp (a, a) ->
   Exp (a, a) ->
   Exp Dim2 ->
   SymbPlane (a, a)
rotateStretchMoveCoords rot mov =
   Symb.map
      (let trans =
            Arith.rotateStretchMoveBackPoint
               (Expr.unzip rot) (Expr.unzip mov)
       in  Expr.modify atomIx2 $ \(Vec2 y x) -> trans $ fromSize2 (x,y))
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
   Exp Dim2 ->
   SymbPlane v ->
   SymbPlane (MaskBool, v)
rotateStretchMove vec rot mov sh img =
   let coords = rotateStretchMoveCoords rot mov sh
       (Vec2 heightSrc widthSrc) = Expr.decompose atomDim2 $ Symb.shape img
   in  Symb.zip
         (validCoords (widthSrc, heightSrc) coords)
         (gatherFrac vec img $
          Symb.map (Expr.modify (atom,atom) $ \(x,y) -> ix2 y x) coords)

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
   let (Vec2 height width) = Expr.decompose atomDim2 $ Symb.shape img
       ((left, right), (top, bottom)) =
         Arith.boundingBoxOfRotatedGen (Expr.min, Expr.max)
            (Expr.unzip rot) (fromSize2 (width, height))
   in  Symb.map Expr.snd $
       rotateStretchMove vec rot (Expr.zip (-left) (-top))
         (dim2 (ceilingToInt (bottom-top)) (ceilingToInt (right-left)))
         img


runRotate :: IO (Degree Float -> ColorImage8 -> IO ColorImage8)
runRotate = do
   rot <-
      RenderP.run $ \rot ->
         colorImageByteFromFloat . rotate vecYUV rot . colorImageFloatFromByte
   return $ \ angle img -> rot (Degree.cis angle) img


brightnessValue :: Exp (YUV a) -> Exp a
brightnessValue = Expr.modify (atom,atom,atom) fst3

brightnessPlane ::
   (Symb.C array, Shape.C size) =>
   array size (YUV a) -> array size a
brightnessPlane = Symb.map brightnessValue

rowHistogram ::
   (Symb.C array, MultiValue.Additive a) =>
   array Dim2 (YUV a) -> array Dim1 a
rowHistogram =
   Symb.fold1 Expr.add .
   ShapeDep.backpermute
      (Expr.modify atomDim2 $ \(Vec2 h w) -> (h,w))
      (Expr.modify (atom,atom) $ \(y,x) -> Vec2 y x) .
   brightnessPlane


tailArr :: (Symb.C array) => array Dim1 a -> array Dim1 a
tailArr = ShapeDep.backpermute (Expr.max 0 . flip Expr.sub 1) (Expr.add 1)

differentiate ::
   (Symb.C array, MultiValue.Additive a) => array Dim1 a -> array Dim1 a
differentiate xs = Symb.zipWith Expr.sub (tailArr xs) xs

the :: Symb.Array () a -> Exp a
the = Symb.the

fold1All ::
   (Shape.C sh, MultiValue.C a) =>
   (Exp a -> Exp a -> Exp a) -> Symb.Array sh a -> Exp a
fold1All f = the . Symb.fold1All f

scoreHistogram :: (MultiValue.PseudoRing a) => Symb.Array Dim1 a -> Exp a
scoreHistogram = fold1All Expr.add . Symb.map Expr.sqr . differentiate


runScoreRotation :: IO (Degree Float -> ColorImage8 -> IO Float)
runScoreRotation = do
   rot <-
      RenderP.run $ \rot ->
         rowHistogram . rotate vecYUV rot . colorImageFloatFromByte
   score <- RenderP.run scoreHistogram
   return $ \ angle img -> score =<< rot (Degree.cis angle) img

findOptimalRotation :: IO ([Degree Float] -> ColorImage8 -> IO (Degree Float))
findOptimalRotation = do
   scoreRotation <- runScoreRotation
   return $ \angles pic ->
      fmap (fst . List.maximumBy (comparing snd)) $
      forM angles $ \angle -> (,) angle <$> scoreRotation angle pic



transpose :: SymbPlane a -> SymbPlane a
transpose =
   ShapeDep.backpermute
      (Expr.modify atomDim2 $ \(Vec2 height width) -> (Vec2 width height))
      (Expr.modify atomIx2 $ \(Vec2 x y) -> (Vec2 y x))

lowpassVert, lowpass ::
   (MultiValue.Field a, MultiValue.Real a, MultiValue.RationalConstant a) =>
   SymbPlane a -> SymbPlane a
lowpassVert img =
   let height = verticalVal $ Symb.shape img
   in  generate (Symb.shape img) $ Expr.modify atomIx2 $ \(Vec2 y x) ->
         Arith.smooth3
            (img ! ix2 (Expr.max 0 (y-1)) x,
             img ! ix2 y x,
             img ! ix2 (Expr.min (height-1) (y+1)) x)

lowpass = transpose . lowpassVert . transpose . lowpassVert

lowpassMulti :: IO (Int -> Plane Float -> IO (Plane Float))
lowpassMulti = do
   lp <- RenderP.run lowpass
   return $ \n -> MonadHT.nest n lp


highpassMulti :: IO (Int -> Plane Float -> IO (Plane Float))
highpassMulti = do
   lp <- lowpassMulti
   sub <- RenderP.run $ Symb.zipWith Expr.sub . fixArray
   return $ \n img -> sub img =<< lp n img



-- counterpart to 'clip'
pad ::
   (MultiValue.C a) =>
   Exp a -> Exp Dim2 -> SymbPlane a -> SymbPlane a
pad a sh img =
   let Vec2 height width = Expr.decompose atomDim2 $ Symb.shape img
   in  generate sh $ \p ->
         let Vec2 y x = Expr.decompose atomIx2 p
         in  Expr.ifThenElse (y<*height &&* x<*width) (img ! p) a

mapPairInt :: (Integral i, Integral j) => (i,i) -> (j,j)
mapPairInt = mapPair (fromIntegral, fromIntegral)


liftCArray2 ::
   (SV.Storable a) =>
   (CArray (Int,Int) a -> CArray (Int,Int) a -> CArray (Int,Int) a) ->
   Plane a -> Plane a -> IO (Plane a)
liftCArray2 f a b =
   arrayKneadFromC <$>
   liftM2 f
      (arrayCFromKnead a)
      (arrayCFromKnead b)


fixArray :: Id (Symb.Array sh a)
fixArray = id

prepareOverlapMatching ::
   IO (Int -> (Degree Float, ColorImage8) -> IO ((Float, Float), Plane Float))
prepareOverlapMatching = do
   bright <- RenderP.run $ brightnessPlane . colorImageFloatFromByte . fixArray
   hp <- highpassMulti
   rotat <- RenderP.run $ rotate Arith.vecScalar
   return $ \radius (angle, img) ->
      let Vec2 height width = Phys.shape img
          rot = Degree.cis angle
          ((left, _right), (top, _bottom)) =
            Arith.boundingBoxOfRotated rot
               (fromIntegral width, fromIntegral height)
      in  fmap ((,) (left, top)) $
          rotat rot =<< hp radius =<< bright img


wrap :: Exp Size -> Exp Size -> Exp Size -> Exp Size
wrap size split c = Expr.select (c<*split) c (c-size)

displacementMap ::
   Exp Size -> Exp Size -> Exp Dim2 -> SymbPlane (Size, Size)
displacementMap xsplit ysplit sh =
   let Vec2 height width = Expr.decompose atomDim2 sh
   in  generate sh $ Expr.modify atomIx2 $ \(Vec2 y x) ->
         (wrap width xsplit x, wrap height ysplit y)

attachDisplacements ::
   Exp Size -> Exp Size ->
   SymbPlane a -> SymbPlane (a, (Size, Size))
attachDisplacements xsplit ysplit img =
   Symb.zip img $ displacementMap xsplit ysplit (Symb.shape img)


{- |
Set all scores to zero within a certain border.
Otherwise the matching algorithm will try to match strong bars at the borders
that are actually digitalization artifacts.
-}
minimumOverlapScores ::
   (MultiValue.Select a, MultiValue.PseudoRing a,
    MultiValue.IntegerConstant a, MultiValue.Real a) =>
   ((Exp Size, Exp Size) -> Exp a -> Exp a) ->
   Exp Size -> (Exp Size, Exp Size) -> (Exp Size, Exp Size) ->
   SymbPlane (a, (Size, Size)) ->
   SymbPlane (a, (Size, Size))
minimumOverlapScores weight minOverlap (widtha,heighta) (widthb,heightb) =
   Symb.map
      (Expr.modify (atom,(atom,atom)) $ \(v, dp@(dx,dy)) ->
         let clipWidth  = Expr.min widtha  (widthb  + dx) - Expr.max 0 dx
             clipHeight = Expr.min heighta (heightb + dy) - Expr.max 0 dy
         in  (Expr.select
                  (clipWidth >=* minOverlap  &&*  clipHeight >=* minOverlap)
                  (weight (clipWidth, clipHeight) v) 0,
              dp))


allOverlapsFromCorrelation ::
   Dim2 ->
   Exp Float ->
   Exp Dim2 -> Exp Dim2 -> SymbPlane Float ->
   SymbPlane (Float, (Size, Size))
allOverlapsFromCorrelation (Vec2 height width) minOverlapPortion =
   \sha shb correlated ->
      let (Vec2 heighta widtha) = Expr.decompose atomDim2 sha
          (Vec2 heightb widthb) = Expr.decompose atomDim2 shb
          half = flip Expr.idiv 2
          minOverlap =
             fastRound $
                minOverlapPortion
                *
                fromInt
                   (Expr.min
                      (Expr.min widtha heighta)
                      (Expr.min widthb heightb))
          weight =
             if False
               then \(clipWidth, clipHeight) v ->
                     v / (fromInt clipWidth * fromInt clipHeight)
               else const id
      in  minimumOverlapScores weight minOverlap
             (widtha, heighta) (widthb, heightb) $
          attachDisplacements
             (half $ Expr.fromInteger' (toInteger width) - widthb + widtha)
             (half $ Expr.fromInteger' (toInteger height) - heightb + heighta) $
          correlated


allOverlapsRun ::
   Dim2 -> IO (Float -> Plane Float -> Plane Float -> IO (Plane Word8))
allOverlapsRun padExtent@(Vec2 height width) = do
   run <-
      RenderP.run $ \minOverlapPortion sha shb img ->
         imageByteFromFloat $
         Symb.map (0.0001*) $
         Symb.map Expr.fst $
         allOverlapsFromCorrelation padExtent minOverlapPortion sha shb img

   return $ \overlap a b ->
      run overlap (Phys.shape a) (Phys.shape b)
         =<< liftCArray2 (KneadCArray.correlatePadded $ mapPairInt (height, width)) a b


argmax ::
   (MultiValue.Comparison a, MultiValue.Select a, MultiValue.Select b) =>
   Exp (a, b) -> Exp (a, b) -> Exp (a, b)
argmax x y  =  Expr.select (Expr.fst x <=* Expr.fst y) y x

argmaximum ::
   (Shape.C sh,
    MultiValue.Comparison a, MultiValue.Select a, MultiValue.Select b) =>
   Symb.Array sh (a, b) -> Exp (a, b)
argmaximum = fold1All argmax

optimalOverlap ::
   Dim2 -> IO (Float -> Plane Float -> Plane Float -> IO (Float, (Size, Size)))
optimalOverlap padExtent@(Vec2 height width) = do
   run <-
      RenderP.run $ \minOverlapPortion (sha, shb) img ->
         argmaximum $
         allOverlapsFromCorrelation padExtent minOverlapPortion sha shb img

   return $ \overlap a b ->
      run overlap (Phys.shape a, Phys.shape b)
         =<< liftCArray2 (KneadCArray.correlatePadded $ mapPairInt (height, width)) a b


shrink ::
   (MultiValue.Field a, MultiValue.RationalConstant a, MultiValue.Real a,
    MultiValue.NativeFloating a ar) =>
   Shape2 (Exp Size) -> SymbPlane a -> SymbPlane a
shrink (Vec2 yk xk) =
   Symb.map (/ (fromInt xk * fromInt yk)) .
   Symb.fold1 Expr.add .
   ShapeDep.backpermute
      (Expr.modify atomDim2 $ \(Vec2 height width) ->
         (Vec2 (Expr.idiv height yk) (Expr.idiv width xk), Vec2 yk xk))
      (Expr.modify (atomIx2, atomIx2) $
         \(Vec2 yi xi, Vec2 yj xj) -> Vec2 (yi*yk+yj) (xi*xk+xj))

shrinkFactors :: (Integral a) => Dim2 -> Shape2 a -> Shape2 a -> Shape2 a
shrinkFactors (Vec2 heightPad widthPad)
   (Vec2 heighta widtha) (Vec2 heightb widthb) =
      Vec2
         (Arith.divUp (heighta+heightb) $ fromIntegral heightPad)
         (Arith.divUp (widtha +widthb)  $ fromIntegral widthPad)


optimalOverlapBig ::
   Dim2 -> IO (Float -> Plane Float -> Plane Float -> IO (Float, (Size, Size)))
optimalOverlapBig padExtent = do
   shrnk <- RenderP.run $ shrink . Expr.decompose atomDim2
   optOverlap <- optimalOverlap padExtent
   return $ \minimumOverlap a b -> do
      let factors@(Vec2 yk xk) =
            shrinkFactors padExtent (Phys.shape a) (Phys.shape b)
      aSmall <- shrnk factors a
      bSmall <- shrnk factors b
      mapSnd (mapPair ((*xk), (*yk))) <$>
         optOverlap minimumOverlap aSmall bSmall


clip ::
   (MultiValue.C a) =>
   (Exp Size, Exp Size) ->
   (Exp Size, Exp Size) ->
   SymbPlane a -> SymbPlane a
clip (left,top) (width,height) =
   Symb.backpermute
      (Expr.compose $ Vec2 height width)
      (Expr.modify (Vec2 atom atom) $ \(Vec2 y x) -> Vec2 (y+top) (x+left))


overlappingArea ::
   (Ord a, Num a) =>
   Shape2 a ->
   Shape2 a ->
   (a, a) -> ((a, a), (a, a), (a, a))
overlappingArea (Vec2 heighta widtha) (Vec2 heightb widthb) (dx, dy) =
   let left = max 0 dx
       top  = max 0 dy
       right  = min widtha  (widthb  + dx)
       bottom = min heighta (heightb + dy)
       width  = right - left
       height = bottom - top
   in  ((left, top), (right, bottom), (width, height))


{-
Like 'optimalOverlapBig'
but computes precise distance in a second step
using a part in the overlapping area.
-}
optimalOverlapBigFine ::
   Dim2 -> IO (Float -> Plane Float -> Plane Float -> IO (Float, (Size, Size)))
optimalOverlapBigFine padExtent@(Vec2 heightPad widthPad) = do
   overlap <- optimalOverlap padExtent
   -- optimalOverlap is rendered again here
   overlapBig <- optimalOverlapBig padExtent
   clp <- RenderP.run clip
   return $ \minimumOverlap a b -> do
      let shapeA = Phys.shape a
      let shapeB = Phys.shape b
      coarsed@(coarsedx,coarsedy) <- snd <$> overlapBig minimumOverlap a b
      let ((leftOverlap, topOverlap), _,
           (widthOverlap, heightOverlap))
               = overlappingArea shapeA shapeB coarsed
          widthFocus  = min widthOverlap $ div widthPad 2
          heightFocus = min heightOverlap $ div heightPad 2
          extentFocus = (widthFocus,heightFocus)
          leftFocus = leftOverlap + div (widthOverlap-widthFocus) 2
          topFocus  = topOverlap  + div (heightOverlap-heightFocus) 2
          addCoarsePos (xm,ym) = (xm+coarsedx, ym+coarsedy)
      clipA <- clp (leftFocus,topFocus) extentFocus a
      clipB <- clp (leftFocus-coarsedx,topFocus-coarsedy) extentFocus b
      mapSnd addCoarsePos <$> overlap minimumOverlap clipA clipB


{-
Like 'optimalOverlapBigFine'
but computes precise distances between many point pairs in a second step
using many parts in the overlapping area.
These point correspondences
can be used to compute corrections to rotation angles.
-}
optimalOverlapBigMulti ::
   Dim2 -> Dim2 -> Int ->
   IO (Float -> Maybe Float -> Plane Float -> Plane Float ->
       IO [(Float, (Size, Size), (Size, Size))])
optimalOverlapBigMulti padExtent (Vec2 heightStamp widthStamp) numCorrs = do
   shrnk <- RenderP.run $ shrink . Expr.decompose atomDim2
   optOverlap <- optimalOverlap padExtent
   overDiff <- overlapDifferenceRun
   clp <- RenderP.run clip

   optOverlapFine <- optimalOverlap $ Vec2 (2*heightStamp) (2*widthStamp)
   let overlapFine minimumOverlap a b
         anchorA@(leftA, topA) anchorB@(leftB, topB) extent@(width,height) = do
            let addCoarsePos (score, (xm,ym)) =
                  let xc = div (width+xm) 2
                      yc = div (height+ym) 2
                  in  (score,
                       (leftA+xc,    topA+yc),
                       (leftB+xc-xm, topB+yc-ym))
            clipA <- clp anchorA extent a
            clipB <- clp anchorB extent b
            addCoarsePos <$> optOverlapFine minimumOverlap clipA clipB

   return $ \minimumOverlap mMaximumDiff a b -> do
      let factors@(Vec2 yk xk) =
            shrinkFactors padExtent (Phys.shape a) (Phys.shape b)
      aSmall <- shrnk factors a
      bSmall <- shrnk factors b

      shrunkd@(shrunkdx, shrunkdy)
         <- snd <$> optOverlap minimumOverlap aSmall bSmall
      let coarsedx = shrunkdx * xk
      let coarsedy = shrunkdy * yk
      let coarsed = (coarsedx,coarsedy)

      doesOverlap <-
         case mMaximumDiff of
            Just maximumDiff ->
               (maximumDiff>) <$> overDiff shrunkd aSmall bSmall
            Nothing -> return True

      let ((leftOverlap, topOverlap),
           (rightOverlap, bottomOverlap),
           (widthOverlap, heightOverlap))
             = overlappingArea (Phys.shape a) (Phys.shape b) coarsed

      let widthStampClip = min widthOverlap widthStamp
          heightStampClip = min heightOverlap heightStamp

      (if doesOverlap then id else const $ return []) $
         mapM
            (\(x,y) ->
               overlapFine minimumOverlap a b
                  (x, y) (x-coarsedx, y-coarsedy)
                  (widthStampClip, heightStampClip)) $
         zip
            (map round $ tail $ init $
             Arith.linearScale (numCorrs+1)
                (fromIntegral leftOverlap :: Double,
                 fromIntegral $ rightOverlap - widthStampClip))
            (map round $ tail $ init $
             Arith.linearScale (numCorrs+1)
                (fromIntegral topOverlap :: Double,
                 fromIntegral $ bottomOverlap - heightStampClip))


overlapDifference ::
   (MultiValue.Algebraic a, MultiValue.RationalConstant a,
    MultiValue.Real a, MultiValue.NativeFloating a ar) =>
   (Exp Size, Exp Size) ->
   SymbPlane a -> SymbPlane a -> Exp a
overlapDifference (dx,dy) a b =
   let (Vec2 heighta widtha) = Expr.decompose atomDim2 $ Symb.shape a
       (Vec2 heightb widthb) = Expr.decompose atomDim2 $ Symb.shape b
       leftOverlap = Expr.max 0 dx
       topOverlap  = Expr.max 0 dy
       rightOverlap  = Expr.min widtha  (widthb  + dx)
       bottomOverlap = Expr.min heighta (heightb + dy)
       widthOverlap  = rightOverlap - leftOverlap
       heightOverlap = bottomOverlap - topOverlap
       extentOverlap = (widthOverlap,heightOverlap)
   in  Expr.sqrt $
       (/(fromInt widthOverlap * fromInt heightOverlap)) $
       fold1All (+) $
       Symb.map Expr.sqr $
       Symb.zipWith (-)
          (clip (leftOverlap,topOverlap) extentOverlap a)
          (clip (leftOverlap-dx,topOverlap-dy) extentOverlap b)

overlapDifferenceRun ::
   IO ((Size, Size) -> Plane Float -> Plane Float -> IO Float)
overlapDifferenceRun = RenderP.run overlapDifference


overlap2 ::
   (MultiValue.Field a, MultiValue.Real a, MultiValue.RationalConstant a,
    MultiValue.C v) =>
   VecExp a v ->
   (Exp Size, Exp Size) ->
   (SymbPlane v, SymbPlane v) -> SymbPlane v
overlap2 vec (dx,dy) (a,b) =
   let (Vec2 heighta widtha) = Expr.decompose atomDim2 $ Symb.shape a
       (Vec2 heightb widthb) = Expr.decompose atomDim2 $ Symb.shape b
       left = Expr.min 0 dx; right  = Expr.max widtha  (widthb  + dx)
       top  = Expr.min 0 dy; bottom = Expr.max heighta (heightb + dy)
       width  = right - left
       height = bottom - top
   in  generate (dim2 height width) $ Expr.modify atomIx2 $ \(Vec2 y x) ->
         let xa = x + left; xb = xa-dx
             ya = y + top;  yb = ya-dy
             pa = ix2 ya xa
             pb = ix2 yb xb
             inPicA = inBox (widtha,heighta) (xa,ya)
             inPicB = inBox (widthb,heightb) (xb,yb)
         in  Expr.ifThenElse inPicA
               (Expr.ifThenElse inPicB
                  (Arith.vecScale vec (1/2) $ Arith.vecAdd vec (a!pa) (b!pb))
                  (a!pa))
               (Expr.ifThenElse inPicB (b!pb) (Arith.vecZero vec))

composeOverlap ::
   IO ((Size, Size) ->
       ((Degree Float, ColorImage8), (Degree Float, ColorImage8)) ->
       IO ColorImage8)
composeOverlap = do
   over <-
      RenderP.run $ \displacement (ra, picA) (rb, picB) ->
         colorImageByteFromFloat $
         overlap2 vecYUV displacement
           (rotate vecYUV ra $ colorImageFloatFromByte picA,
            rotate vecYUV rb $ colorImageFloatFromByte picB)
   return $ \displacement ((angleA,picA), (angleB,picB)) ->
      over displacement
         (Degree.cis angleA, picA) (Degree.cis angleB, picB)



emptyCountCanvas :: IO (Dim2 -> IO (Plane (Word32, YUV Float)))
emptyCountCanvas =
   RenderP.run $ \sh -> Symb.fill sh (Expr.zip 0 $ Expr.zip3 0 0 0)


type MaskBool = Word8

maskFromBool :: Exp Bool -> Exp MaskBool
maskFromBool = Expr.liftM $ MultiValue.liftM $ LLVM.zext

boolFromMask :: Exp MaskBool -> Exp Bool
boolFromMask = (/=* 0)

intFromBool :: Exp MaskBool -> Exp Word32
intFromBool = Expr.liftM $ MultiValue.liftM $ LLVM.ext

type RotatedImage = ((Float,Float), (Float,Float), ColorImage8)

addToCountCanvas ::
   (MultiValue.PseudoRing a, MultiValue.NativeFloating a ar) =>
   VecExp a v ->
   SymbPlane (MaskBool, v) ->
   SymbPlane (Word32, v) ->
   SymbPlane (Word32, v)
addToCountCanvas vec =
   Symb.zipWith
      (Expr.modify2 (atom,atom) (atom,atom) $ \(mask, pic) (count, canvas) ->
         (Expr.add (intFromBool mask) count,
          Arith.vecAdd vec canvas $
          Arith.vecScale vec (fromInt $ intFromBool mask) pic))

updateCountCanvas ::
   IO (RotatedImage -> Plane (Word32, YUV Float) ->
       IO (Plane (Word32, YUV Float)))
updateCountCanvas =
   RenderP.run $ \(rot, mov, pic) countCanvas ->
      addToCountCanvas vecYUV
         (rotateStretchMove vecYUV rot mov (Symb.shape countCanvas) $
          colorImageFloatFromByte pic)
         countCanvas

finalizeCountCanvas :: IO ((Plane (Word32, YUV Float)) -> IO ColorImage8)
finalizeCountCanvas =
   RenderP.run $
      colorImageByteFromFloat .
      Symb.map
         (Expr.modify (atom,atom) $ \(count, pixel) ->
            Arith.vecScale vecYUV (recip $ fromInt count) pixel) .
      fixArray


diffAbs :: (MultiValue.Real a) => Exp a -> Exp a -> Exp a
diffAbs = Expr.liftM2 $ \x y -> MultiValue.abs =<< MultiValue.sub x y

diffWithCanvas ::
   IO (RotatedImage -> Plane (YUV Float) -> IO (Plane (MaskBool, Float)))
diffWithCanvas =
   RenderP.run $ \(rot, mov, pic) avg ->
      Symb.zipWith
         (Expr.modify2 (atom,atom) atom $ \(b,x) y ->
            (b, diffAbs (brightnessValue x) (brightnessValue y)))
         (rotateStretchMove vecYUV rot mov (Symb.shape avg) $
          colorImageFloatFromByte pic)
         avg

finalizeCountCanvasFloat ::
   IO ((Plane (Word32, YUV Float)) -> IO (Plane (YUV Float)))
finalizeCountCanvasFloat =
   RenderP.run $
      Symb.map
         (Expr.modify (atom,atom) $ \(count, pixel) ->
            Arith.vecScale vecYUV (recip $ fromInt count) pixel)
      .
      fixArray

emptyCanvas :: IO (Dim2 -> IO ColorImage8)
emptyCanvas = RenderP.run $ \sh -> Symb.fill sh (Expr.zip3 0 0 0)

addMaskedToCanvas ::
   IO (RotatedImage ->
       Plane MaskBool ->
       Plane (YUV Word8) ->
       IO (Plane (YUV Word8)))
addMaskedToCanvas =
   RenderP.run $ \(rot, mov, pic) mask canvas ->
      Symb.zipWith3 Expr.ifThenElse
         (Symb.map boolFromMask mask)
         (Symb.map (yuvByteFromFloat . Expr.snd) $
          rotateStretchMove vecYUV rot mov (Symb.shape canvas) $
          colorImageFloatFromByte pic)
         canvas

updateShapedCanvas ::
   IO (RotatedImage ->
       Plane Float ->
       Plane (Float, YUV Float) ->
       IO (Plane (Float, YUV Float)))
updateShapedCanvas =
   RenderP.run $ \(rot, mov, pic) shape weightCanvas ->
      addToWeightedCanvas vecYUV
         (Symb.zipWith
            (Expr.modify2 atom (atom,atom) $ \s (b,x) -> (fromInt b * s, x))
            shape $
          rotateStretchMove vecYUV rot mov (Symb.shape weightCanvas) $
          colorImageFloatFromByte pic)
         weightCanvas


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
   array (sh, SmallSize) (Bool, a) -> array sh (Bool, a)
maskedMinimum = Symb.fold1 (maybePlus Expr.min)


generate ::
   (Shape.C sh) =>
   Exp sh -> (Exp (Shape.Index sh) -> Exp b) -> Symb.Array sh b
generate sh f = Symb.map f $ Symb.id sh

type Geometry a = Arith.Geometry Size a

distanceMapBox ::
   (MultiValue.Field a, MultiValue.NativeFloating a ar,
    MultiValue.Real a, MultiValue.RationalConstant a) =>
   Exp Dim2 ->
   Exp (Geometry a) ->
   SymbPlane (Bool, (((a,(a,a)), (a,(a,a))), ((a,(a,a)), (a,(a,a)))))
distanceMapBox sh geom =
   let (rot, mov, extent@(width,height)) =
         Expr.decompose ((atom,atom),(atom,atom),(atom,atom)) geom
       widthf  = fromInt width
       heightf = fromInt height
       back  = Arith.rotateStretchMoveBackPoint rot mov
       forth = Arith.rotateStretchMovePoint rot mov
   in  generate sh $ Expr.modify atomIx2 $ \(Vec2 y x) ->
         let (xsrc,ysrc) = back $ fromSize2 (x,y)
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
distance a b = Expr.sqrt $ Arith.distanceSqr a b

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
isZero = (==* Expr.zero)

expEven ::
   (MultiValue.Comparison i, MultiValue.Integral i,
    MultiValue.IntegerConstant i) =>
   Exp i -> Exp Bool
expEven = isZero . flip Expr.irem 2

separateDistanceMap ::
   (Symb.C array, Shape.C sh, MultiValue.C a) =>
   array sh (bool, ((a, a), (a, a))) ->
   array (sh, SmallSize) (bool, a)
separateDistanceMap array =
   outerProduct
      (Expr.modify2 (atom, ((atom, atom), (atom, atom))) atom $
       \(b,(horiz,vert)) sel ->
          (b,
           Expr.ifThenElse (expEven $ Expr.idiv sel 2)
               (uncurry (Expr.ifThenElse (expEven sel)) horiz)
               (uncurry (Expr.ifThenElse (expEven sel)) vert)))
      array (Symb.lift0 $ Symb.id 4)

distanceMapBoxRun ::
   IO (Dim2 -> Geometry Float -> IO (Plane Word8))
distanceMapBoxRun =
   RenderP.run $ \sh geom ->
      scaleDistanceMapGeom geom $
      Symb.map
         (Expr.modify (atom,atom) $ \(valid, dist) -> Expr.select valid dist 0) $
      maskedMinimum $
      Symb.map (Expr.mapSnd Expr.fst) $
      separateDistanceMap $
      distanceMapBox sh geom


containedAnywhere ::
   (Symb.C array, Shape.C sh,
    MultiValue.Field a, MultiValue.NativeFloating a ar,
    MultiValue.Real a, MultiValue.RationalConstant a) =>
   array SmallSize (Geometry a) ->
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
   Exp Dim2 ->
   Exp (Geometry a) ->
   Symb.Array SmallSize (Geometry a) ->
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

distanceMapContainedRun ::
   IO (Dim2 -> Geometry Float -> [Geometry Float] -> IO (Plane Word8))
distanceMapContainedRun = do
   distances <-
      RenderP.run $
      \sh this -> scaleDistanceMapGeom this . distanceMapContained sh this
   return $ \sh this others -> distances sh this =<< Phys.vectorFromList others

scaleDistanceMapGeom ::
   (MultiValue.Field a, MultiValue.Real a, MultiValue.RationalConstant a,
    MultiValue.NativeFloating a ar) =>
   Exp (Geometry b) -> SymbPlane a -> SymbPlane Word8
scaleDistanceMapGeom geom img =
   let scale = (4/) $ fromInt $ Expr.uncurry Expr.min $ Expr.thd3 geom
   in  imageByteFromFloat $ Symb.map (scale*) img


pixelCoordinates ::
   (MultiValue.NativeFloating a ar) => Exp Dim2 -> SymbPlane (a,a)
pixelCoordinates sh =
   generate sh $ Expr.modify atomIx2 $ \(Vec2 y x) -> fromSize2 (x,y)

distanceMapPoints ::
   (Shape.C sh, Symb.C array,
    MultiValue.Real a, MultiValue.Algebraic a, MultiValue.IntegerConstant a) =>
   array sh (a,a) ->
   array SmallSize (a,a) ->
   array sh a
distanceMapPoints a b =
   Symb.fold1 Expr.min $
   outerProduct (Expr.modify2 (atom,atom) (atom,atom) distance) a b

distanceMapPointsRun ::
   IO (Dim2 -> [Arith.Point2 Float] -> IO (Plane Word8))
distanceMapPointsRun = do
   distances <-
      RenderP.run $
      \sh -> scaleDistanceMap . distanceMapPoints (pixelCoordinates sh)
   return $ \sh points -> distances sh =<< Phys.vectorFromList points


scaleDistanceMap ::
   (MultiValue.Field a, MultiValue.Real a, MultiValue.RationalConstant a,
    MultiValue.NativeFloating a ar) =>
   SymbPlane a -> SymbPlane Word8
scaleDistanceMap img =
   let scale =
         case Expr.decompose atomDim2 $ Symb.shape img of
            Vec2 y x -> 4 / fromInt (Expr.min x y)
   in  imageByteFromFloat $ Symb.map (scale*) img


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
   Exp Dim2 ->
   Exp (Geometry a) ->
   Symb.Array SmallSize (Geometry a) ->
   Symb.Array SmallSize (a, a) ->
   SymbPlane a
distanceMap sh this others points =
   Symb.zipWith Expr.min
      (distanceMapContained sh this others)
      (distanceMapPoints (pixelCoordinates sh) points)

distanceMapRun ::
   IO (Dim2 ->
       Geometry Float ->
       [Geometry Float] ->
       [Arith.Point2 Float] ->
       IO (Plane Word8))
distanceMapRun = do
   distances <-
      RenderP.run $
      \sh this others -> scaleDistanceMap . distanceMap sh this others
   return $ \sh this others points -> do
      othersVec <- Phys.vectorFromList others
      pointsVec <- Phys.vectorFromList points
      distances sh this othersVec pointsVec


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
   Exp Dim2 ->
   Exp (Geometry a) ->
   Symb.Array SmallSize (Geometry a) ->
   Symb.Array SmallSize (a, a) ->
   SymbPlane a
distanceMapGamma gamma sh this others points =
   Symb.map (pow gamma) $ distanceMap sh this others points


emptyWeightedCanvas :: IO (Dim2 -> IO (Plane (Float, YUV Float)))
emptyWeightedCanvas =
   RenderP.run $ \sh -> Symb.fill sh (Expr.zip 0 $ Expr.zip3 0 0 0)

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

updateWeightedCanvas ::
   IO (Float ->
       Geometry Float ->
       [Geometry Float] ->
       [Arith.Point2 Float] ->
       ColorImage8 ->
       Plane (Float, YUV Float) ->
       IO (Plane (Float, YUV Float)))
updateWeightedCanvas = do
   distances <- RenderP.run distanceMapGamma

   update <-
      RenderP.run $ \this pic dist weightSumCanvas ->
            let (rot, mov, _) = Expr.unzip3 this
            in  addToWeightedCanvas vecYUV
                  (Symb.zip dist $
                   Symb.map Expr.snd $
                   rotateStretchMove vecYUV rot mov
                      (Symb.shape weightSumCanvas) $
                   colorImageFloatFromByte pic)
                  weightSumCanvas

   return $ \gamma this others points pic weightSumCanvas -> do
      othersVec <- Phys.vectorFromList others
      pointsVec <- Phys.vectorFromList points
      dists <-
         distances
            gamma (Phys.shape weightSumCanvas) this
            othersVec pointsVec
      update this pic dists weightSumCanvas


finalizeWeightedCanvas :: IO (Plane (Float, YUV Float) -> IO ColorImage8)
finalizeWeightedCanvas =
   RenderP.run $
      colorImageByteFromFloat .
      Symb.map
         (Expr.modify (atom,atom) $ \(weightSum, pixel) ->
            Arith.vecScale vecYUV (recip weightSum) pixel) .
      fixArray


data
   Picture param =
      Picture {
         picPath :: FilePath,
         picParam :: param,
         picColored :: (Degree Float, ColorImage8),
         picPlane :: ((Float, Float), Plane Float)
      }

mapPicParam :: (a -> b) -> Picture a -> Picture b
mapPicParam f pic = pic{picParam = f $ picParam pic}


processOverlap ::
   Option.Args ->
   IO ([FilePath], [(Degree Float, ColorImage8)],
       [((Float, Float), Complex Float)])
processOverlap args = do
   let opt = Option.option args
   let info = CmdLine.info (Option.verbosity opt)

   pics <-
      map (mapPicParam (\(State.NoAngleCorrection, pos) -> pos)) <$>
      processRotation args
   let padSize = fromIntegral $ Option.padSize opt
   (maybeAllOverlapsShared, optimalOverlapShared) <-
      case Just $ Vec2 padSize padSize of
         Just padExtent -> do
            overlap <- optimalOverlapBigFine padExtent
            return (Nothing, overlap (Option.minimumOverlap opt))
         Nothing -> do
            let padExtent =
                   uncurry Vec2 $ swap $
                   Arith.correlationSize (Option.minimumOverlap opt) $
                   map (colorImageExtent . snd . picColored) pics
            overlap <- optimalOverlap padExtent
            allOverlapsIO <- allOverlapsRun padExtent
            return
               (Just $ allOverlapsIO (Option.minimumOverlap opt),
                overlap (Option.minimumOverlap opt))

   relations <-
      maybe (return Map.empty)
         (State.readDisplacement (map picPath pics))
         (Option.relations opt)

   composeOver <- composeOverlap
   overlapDiff <- overlapDifferenceRun
   let open = map ((\(mx,my) -> isNothing mx || isNothing my) . picParam) pics
   displacements <-
      forM (guardedPairs open $ zip [0..] pics) $
            \((ia, Picture pathA _ origA (leftTopA,picA)),
              (ib, Picture pathB _ origB (leftTopB,picB))) -> do
         forM_ maybeAllOverlapsShared $ \allOverlapsShared -> when False $
            writeGrey (Option.quality opt)
               (printf "/tmp/%s-%s-score.jpeg"
                  (FilePath.takeBaseName pathA) (FilePath.takeBaseName pathB))
            =<< allOverlapsShared picA picB

         let relation = Map.lookup (pathA,pathB) relations
         md <-
            case (join $ fmap fst relation, join $ fmap snd relation) of
               (Just State.NonOverlapping, _) -> return Nothing
               (Just State.Overlapping, Just d) -> return $ Just d
               (related, _) -> do
                  doffset@(dox,doy) <- snd <$> optimalOverlapShared picA picB
                  diff <- overlapDiff doffset picA picB
                  let overlapping =
                        related == Just State.Overlapping
                        ||
                        diff < Option.maximumDifference opt
                  let d = (fromIntegral dox + fst leftTopA - fst leftTopB,
                           fromIntegral doy + snd leftTopA - snd leftTopB)
                  info $
                     printf "%s - %s, %s, difference %f%s\n"
                        pathA pathB (show d) diff
                        (if overlapping then "" else " unrelated -> ignoring")
                  forM_ (Option.outputOverlap opt) $ \format ->
                     writeImage (Option.quality opt)
                        (printf format
                           (FilePath.takeBaseName pathA)
                           (FilePath.takeBaseName pathB))
                     =<< composeOver doffset (origA, origB)
                  return $ toMaybe overlapping d
         return ((ia,ib), (pathA,pathB), md)

   forM_ (Option.outputState opt) $ \format ->
      State.writeDisplacement (printf format "relation") displacements

   let overlaps = mapMaybe (\(i,_paths,md) -> (,) i <$> md) displacements
   let (poss, dps) =
          absolutePositionsFromPairDisplacements
             (fixAtLeastOnePosition (0,0) $ map picParam pics) overlaps
   info "\nabsolute positions"
   info $ unlines $ map show poss

   info "\ncompare position differences with pair displacements"
   info $ unlines $
      zipWith
         (\(dpx,dpy) (dx,dy) ->
            printf "(%f,%f) (%f,%f)" dpx dpy dx dy)
         dps (map snd overlaps)
   let (errdx,errdy) =
          mapPair (maximum0, maximum0) $ unzip $
          zipWith
             (\(dpx,dpy) (dx,dy) ->
                (abs $ dpx - realToFrac dx, abs $ dpy - realToFrac dy))
             dps (map snd overlaps)

   info $
      "\n"
      ++
      printf "maximum horizontal error: %f\n" errdx
      ++
      printf "maximum vertical error: %f\n" errdy

   return
      (map picPath pics,
       map picColored pics,
       map (flip (,) 1) $ map (mapPair (realToFrac, realToFrac)) poss)


processOverlapRotate ::
   Option.Args ->
   IO ([FilePath], [(Degree Float, ColorImage8)],
       [((Float, Float), Complex Float)])
processOverlapRotate args = do
   let opt = Option.option args
   let info = CmdLine.info (Option.verbosity opt)
   let infoPlain = when (Option.verbosity opt >= Verbosity.verbose) . putStr

   pics <-
      map (mapPicParam (mapFst State.getAngleCorrection)) <$>
      processRotation args
   let padSize = Option.padSize opt
   let stampSize = Option.stampSize opt
   optimalOverlapShared <-
      optimalOverlapBigMulti
         (shape2 padSize padSize)
         (shape2 stampSize stampSize)
         (Option.numberStamps opt)
      <*> pure (Option.minimumOverlap opt)

   relations <-
      maybe (return Map.empty)
         (State.readRotated (map picPath pics))
         (Option.relations opt)

   let open =
         map
            ((\(ma, (mx,my)) -> isNothing ma || isNothing mx || isNothing my)
             . picParam)
            pics
   displacements <-
      forM (guardedPairs open $ zip [0..] pics) $
            \((ia, Picture pathA _ _ (leftTopA,picA)),
              (ib, Picture pathB _ _ (leftTopB,picB))) -> do
         let relation = Map.lookup (pathA,pathB) relations
         correspondences <-
            case (join $ fmap fst relation, Fold.fold $ fmap snd relation) of
               (Just State.NonOverlapping, _) -> return []
               (Just State.Overlapping, corrs@(_:_)) -> return corrs
               (related, _) -> do
                  let add (x0,y0) (x1,y1) =
                        (fromIntegral x0 + x1, fromIntegral y0 + y1)
                  let mMaxDiff =
                        toMaybe (related /= Just State.Overlapping) $
                        Option.maximumDifference opt
                  corrs <-
                     map
                        (\(score,pa,pb) ->
                           (score, (add pa leftTopA, add pb leftTopB))) <$>
                     optimalOverlapShared mMaxDiff picA picB
                  info $ printf "left-top: %s, %s" (show leftTopA) (show leftTopB)
                  info $ printf "%s - %s" pathA pathB
                  forM_ corrs $ \(score, (pa@(xa,ya),pb@(xb,yb))) ->
                     info $
                        printf "%s ~ %s, (%f,%f), %f"
                           (show pa) (show pb) (xb-xa) (yb-ya) score
                  return $ map snd corrs
         return ((ia,ib), (pathA,pathB), correspondences)

   forM_ (Option.outputState opt) $ \format ->
      State.writeRotated (printf format "relation") displacements

   let overlaps = concatMap (\(i,_paths,ps) -> (,) i <$> ps) displacements
   let (posRots, dps) =
          layoutFromPairDisplacements
             (map (mapFst (fmap Degree.cis)) $
              fixAtLeastOneAnglePosition (Degree 0, (0,0)) $
              map picParam pics)
             overlaps
   info "\nabsolute positions and rotations: place, rotation (magnitude, phase)"
   infoPlain $ unlines $
      map
         (\((dx,dy),r) ->
            printf "(%8.2f,%8.2f), %8.6f :+ %9.6f (%8.6f, %7.3f)" dx dy
               (Complex.realPart r) (Complex.imagPart r)
               (Complex.magnitude r)
               (getDegree $ Degree.fromRadian $ Complex.phase r))
         posRots

   info "\ncompare position differences with pair displacements"
   infoPlain $ unlines $
      zipWith
         (\(dpx,dpy) (_i, ((xa,ya),(xb,yb))) ->
            printf "(%8.5f,%8.5f) (%7.2f,%7.2f) ~ (%7.2f,%7.2f)"
               dpx dpy xa ya xb yb)
         dps overlaps

   return
      (map picPath pics,
       map picColored pics,
       map
         (mapPair
            (mapPair (realToFrac, realToFrac), Komplex.map realToFrac))
         posRots)


processRotation ::
   (State.AngleCorrected angleCorr) =>
   Option.Args -> IO [Picture (angleCorr, (Maybe Float, Maybe Float))]
processRotation args = do
   let opt = Option.option args
   let notice = CmdLine.notice (Option.verbosity opt)
   let info = CmdLine.info (Option.verbosity opt)

   inputs <- Option.images args

   notice "\nfind rotation angles"
   findOptRot <- findOptimalRotation
   picAngles <-
      forM inputs $ \(State.Proposed path (maybeAngle, _) _) -> do
         pic <- readImage (Option.verbosity opt) path
         let maxAngle = Option.maximumAbsoluteAngle opt
         let angles = Degree.linearScale (Option.numberAngleSteps opt) maxAngle
         angle <-
            case maybeAngle of
               Just angle -> return angle
               Nothing -> findOptRot angles pic
         info $ printf "%s %f\176\n" path (getDegree angle)
         return (angle, pic)

   forM_ (Option.outputState opt) $ \format ->
      State.write (printf format "angle") $
         zipWith State.Angle (map State.propPath inputs) (map fst picAngles)

   notice "\nfind relative placements"
   prepOverlapMatching <- prepareOverlapMatching
   rotated <- mapM (prepOverlapMatching (Option.smooth opt)) picAngles

   when False $ do
      notice "write fft"
      let pic0 : pic1 : _ = map snd rotated
          size = (1024,768)
      cpic0 <- arrayCFromKnead pic0
      cpic1 <- arrayCFromKnead pic1
      makeByteImage <-
         RenderP.run $ \k -> imageByteFromFloat . Symb.map (k*) . fixArray
      writeGrey (Option.quality opt) "/tmp/padded.jpeg" =<<
         (makeByteImage 1 $ arrayKneadFromC $ KneadCArray.pad 0 size cpic0)
      writeGrey (Option.quality opt) "/tmp/spectrum.jpeg" =<<
         (makeByteImage 0.1 $ arrayKneadFromC $
          CArray.liftArray Complex.magnitude $
          FFT.dftRCN [0,1] $ KneadCArray.pad 0 size cpic0)
      writeGrey (Option.quality opt) "/tmp/convolution.jpeg" =<<
         (makeByteImage 0.1 $ arrayKneadFromC $
          KneadCArray.correlatePadded size cpic0 cpic1)

   return $
      zipWith3
         (\(State.Proposed path (_,angleCorr) maybePos) colored plane ->
            Picture path (angleCorr, maybePos) colored plane)
         inputs picAngles rotated

process :: Option.Args -> IO ()
process args = do
   IO.hSetBuffering IO.stdout IO.LineBuffering
   IO.hSetBuffering IO.stderr IO.LineBuffering

   let opt = Option.option args
   let notice = CmdLine.notice (Option.verbosity opt)
   let info = CmdLine.info (Option.verbosity opt)

   (paths, picAngles, posRots) <-
      if Option.finetuneRotate opt
        then processOverlapRotate args
        else processOverlap args

   forM_ (Option.outputState opt) $ \format ->
      State.write (printf format "position") $
      zipWith3
         (\path (angle, _) (pos, rot) ->
            State.Position path
               (angle <> Degree.fromRadian (Complex.phase rot)) pos)
         paths picAngles posRots

   notice "\ncompose all parts"
   let ((canvasWidth, canvasHeight), rotMovPics, canvasMsgs) =
         Arith.canvasShape colorImageExtent
            (map (mapFst Degree.toRadian) picAngles) posRots
   let canvasShape = shape2 canvasHeight canvasWidth
   mapM_ info canvasMsgs

   forM_ (Option.outputHard opt) $ \path -> do
      emptyCanv <- emptyCountCanvas
      updateCanv <- updateCountCanvas
      finalizeCanv <- finalizeCountCanvas

      empty <- emptyCanv canvasShape
      writeImage (Option.quality opt) path =<< finalizeCanv =<<
         foldM (flip updateCanv) empty rotMovPics


   notice "\ndistance maps"
   let geometryRelations =
         Arith.geometryRelations $
         map (Arith.geometryFeatures . mapThd3 colorImageExtent) rotMovPics

   forM_ (Option.outputDistanceMap opt) $ \format -> do
      debug <-
         if True
           then return $ \ _stem _geoms -> return ()
           else do
            distMapBox <- distanceMapBoxRun
            distMapContained <- distanceMapContainedRun
            distMapPoints <- distanceMapPointsRun

            return $ \stem (thisGeom, otherGeoms, allPoints) -> do
               writeGrey (Option.quality opt)
                  (printf "/tmp/%s-distance-box.jpeg" stem)
                  =<< distMapBox canvasShape thisGeom

               writeGrey (Option.quality opt)
                  (printf "/tmp/%s-distance-contained.jpeg" stem)
                  =<< distMapContained canvasShape thisGeom otherGeoms

               writeGrey (Option.quality opt)
                  (printf "/tmp/%s-distance-points.jpeg" stem)
                  =<< distMapPoints canvasShape allPoints

      distMap <- distanceMapRun
      forM_ (zip geometryRelations paths) $ \(geoms, path) -> do
         let stem = FilePath.takeBaseName path
         debug stem geoms
         writeGrey (Option.quality opt) (printf format stem) =<<
            uncurry3 (distMap canvasShape) geoms

   forM_ (Option.output opt) $ \path -> do
      notice "\nweighted composition"
      emptyCanv <- emptyWeightedCanvas
      updateCanv <- updateWeightedCanvas
      finalizeCanv <- finalizeWeightedCanvas

      empty <- emptyCanv canvasShape
      writeImage (Option.quality opt) path =<< finalizeCanv =<<
         foldM
            (\canvas ((thisGeom, otherGeoms, allPoints), (_rot, pic)) ->
               updateCanv (Option.distanceGamma opt)
                  thisGeom otherGeoms allPoints pic canvas)
            empty (zip geometryRelations picAngles)

   when (isJust (Option.outputShaped opt) || isJust (Option.outputShapedHard opt)) $ do
      notice "\nmatch shapes"
      emptyCanv <- emptyCountCanvas
      updateCanv <- updateCountCanvas
      finalizeCanv <- finalizeCountCanvasFloat

      empty <- emptyCanv canvasShape
      sumImg <- foldM (flip updateCanv) empty rotMovPics

      avg <- finalizeCanv sumImg
      diff <- diffWithCanvas
      picDiffs <- mapM (flip diff avg) rotMovPics
      getSnd <- RenderP.run $ Symb.map Expr.snd . fixArray
      lp <- lowpassMulti
      masks <- map (amap ((0/=) . fst)) <$> mapM arrayCFromKnead picDiffs
      let smoothRadius = Option.shapeSmooth opt
      smoothPicDiffs <-
         mapM (arrayCFromKnead <=< lp smoothRadius <=< getSnd) picDiffs
      (locs, pqueue) <-
         MatchImageBorders.prepareShaping $ zip masks smoothPicDiffs
      counts <- thaw . amap (fromIntegral . fst) =<< arrayCFromKnead sumImg
      shapes <- MatchImageBorders.shapeParts counts locs pqueue

      let names = map FilePath.takeBaseName paths
      forM_ (Option.outputShapedHard opt) $ \path -> do
         forM_ (Option.outputShapeHard opt) $ \format ->
            forM_ (zip names shapes) $ \(name,shape) ->
               writeGrey (Option.quality opt) (printf format name) $
               arrayKneadFromC $ amap (\b -> if b then 255 else 0) shape

         emptyPlainCanv <- emptyCanvas
         addMasked <- addMaskedToCanvas
         emptyPlain <- emptyPlainCanv canvasShape
         writeImage (Option.quality opt) path =<<
            foldM
               (\canvas (shape, rotMovPic) ->
                  addMasked rotMovPic
                     (arrayKneadFromC $ amap (fromIntegral . fromEnum) shape)
                     canvas)
               emptyPlain (zip shapes rotMovPics)

      forM_ (Option.outputShaped opt) $ \path -> do
         smoothShapes <-
            mapM
               (lp smoothRadius . arrayKneadFromC .
                amap (fromIntegral . fromEnum))
               shapes
         forM_ (Option.outputShape opt) $ \format -> do
            makeByteImage <- RenderP.run $ imageByteFromFloat . fixArray
            forM_ (zip names smoothShapes) $ \(name,shape) ->
               writeGrey (Option.quality opt) (printf format name)
                  =<< makeByteImage shape

         emptyWeightedCanv <- emptyWeightedCanvas
         updateWeightedCanv <- updateShapedCanvas
         finalizeWeightedCanv <- finalizeWeightedCanvas
         emptyWeighted <- emptyWeightedCanv canvasShape
         writeImage (Option.quality opt) path =<<
            finalizeWeightedCanv =<<
            foldM
               (\canvas (shape, rotMovPic) ->
                  updateWeightedCanv rotMovPic shape canvas)
               emptyWeighted (zip smoothShapes rotMovPics)


rotateTest :: IO ()
rotateTest = do
   rot <- runRotate
   img <- readImage Verbosity.normal "/tmp/bild/artikel0005.jpeg"
   forM_ [0..11] $ \k -> do
      let path = printf "/tmp/rotated/%04d.jpeg" k
      putStrLn path
      writeImage 100 path =<< rot (Degree $ fromInteger k * 30) img

scoreTest :: IO ()
scoreTest = do
   score <- runScoreRotation
   img <- readImage Verbosity.normal "/tmp/bild/artikel0005.jpeg"
   forM_ [-10..10] $ \k -> do
      print =<< score (Degree $ fromInteger k / 10) img

main :: IO ()
main = process =<< Option.get Option.Knead
