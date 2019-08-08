module Option where

import qualified State
import Shell.Utility.ParseArgument (parseNumber)
import Shell.Utility.Exit (exitFailureMsg)
import Shell.Utility.GetOpt (fmapOptDescr)
import Degree (Degree(Degree, getDegree))

import qualified System.Console.GetOpt as Opt
import qualified System.Environment as Env
import System.Console.GetOpt (ArgDescr(NoArg, ReqArg), getOpt, usageInfo)

import qualified System.Exit as Exit
import qualified System.IO as IO

import Control.Monad (when)

import qualified Data.EnumBitSet as EnumSet
import qualified Data.Vector as Vector
import qualified Data.Map as Map
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Data.Tuple.HT (mapSnd)
import Data.Monoid ((<>))
import Data.Word (Word8)

import qualified Shell.Utility.Verbosity as Verbosity
import Shell.Utility.Verbosity (Verbosity)

import Text.Printf (printf)


data Args =
   Args {
      option :: Option,
      inputs :: [(Image, FilePath)]
   }

defltArgs :: Args
defltArgs = Args {option = defltOption, inputs = []}


data Option =
   Option {
      verbosity :: Verbosity,
      state :: Maybe FilePath,
      relations :: Maybe FilePath,
      output :: Maybe FilePath,
      outputHard :: Maybe FilePath,
      outputShaped :: Maybe FilePath,
      outputShapedHard :: Maybe FilePath,
      outputOverlap :: Maybe String,
      outputDistanceMap :: Maybe String,
      outputShape :: Maybe String,
      outputShapeHard :: Maybe String,
      outputState :: Maybe String,
      quality :: Int,
      maximumAbsoluteAngle :: Degree Float,
      numberAngleSteps :: Int,
      radonTransform :: Bool,
      smooth :: Int,
      padSize :: Int,
      minimumOverlap :: Float,
      maximumDifference :: Float,
      finetuneRotate :: Bool,
      numberStamps :: Int,
      stampSize :: Int,
      distanceGamma :: Float,
      shapeSmooth :: Int
   }

defltOption :: Option
defltOption =
   Option {
      verbosity = Verbosity.verbose,
      state = Nothing,
      relations = Nothing,
      output = Nothing,
      outputHard = Nothing,
      outputShaped = Nothing,
      outputShapedHard = Nothing,
      outputOverlap = Nothing,
      outputDistanceMap = Nothing,
      outputShape = Nothing,
      outputShapeHard = Nothing,
      outputState = Nothing,
      quality = 99,
      maximumAbsoluteAngle = Degree 1,
      numberAngleSteps = 40,
      radonTransform = False,
      smooth = 20,
      padSize = 1024,
      minimumOverlap = 1/4,
      maximumDifference = 0.2,
      finetuneRotate = False,
      numberStamps = 5,
      stampSize = 64,
      distanceGamma = 2,
      shapeSmooth = 200
   }


data Image =
   Image {
      angle :: Maybe (Degree Float)
   }
   deriving (Eq)

defltImage :: Image
defltImage = Image {angle = Nothing}

proposedFromImage ::
   (State.AngleCorrected angleCorr) =>
   (Image, FilePath) -> State.Proposed angleCorr
proposedFromImage (Image ang, path) =
   State.Proposed path (ang, State.autoAngleCorrection) (Nothing, Nothing)


data Engine = Knead | Accelerate
   deriving (Eq, Ord, Enum)

type EngineSet = EnumSet.T Word8 Engine

knead, accelerate, generic :: EngineSet
knead = EnumSet.singleton Knead
accelerate = EnumSet.singleton Accelerate
generic = knead <> accelerate


type Description a = [Opt.OptDescr (a -> IO a)]
type EngineDescription a = [(EngineSet, Opt.OptDescr (a -> IO a))]

opt ::
   EngineSet -> [Char] -> [String] -> ArgDescr a -> String ->
   (EngineSet, Opt.OptDescr a)
opt engines short long argDescr help =
   (engines, Opt.Option short long argDescr help)

{-
Guide for common Linux/Unix command-line options:
  http://www.faqs.org/docs/artu/ch10s05.html
-}
optionDescription :: Description a -> EngineDescription Option
optionDescription desc =
   opt generic ['h'] ["help"]
      (NoArg $ \ _flags -> do
         programName <- Env.getProgName
         putStrLn $
            usageInfo
               ("Usage: " ++ programName ++
                " [OPTIONS]... [[INPUTOPTIONS]... INPUT]...") $
            desc
         Exit.exitSuccess)
      "show options" :

   opt generic ['v'] ["verbose"]
      (flip ReqArg "N" $ \str flags -> do
         case Verbosity.parse str of
            Right n -> return (flags{verbosity = n})
            Left msg -> exitFailureMsg msg)
      (printf "verbosity level: 0..3, default: %d"
         (fromEnum $ verbosity defltOption)) :

   opt generic [] ["state"]
      (flip ReqArg "PATH" $ \path flags ->
         return $ flags{state = Just path})
      ("CSV file with predefined parameters") :

   opt generic [] ["relations"]
      (flip ReqArg "PATH" $ \str flags ->
         return $ flags{relations = Just str})
      ("CSV file with image pairs") :

   opt generic [] ["output"]
      (flip ReqArg "PATH" $ \str flags ->
         return $ flags{output = Just str})
      ("path to generated collage") :

   opt generic [] ["output-hard"]
      (flip ReqArg "PATH" $ \str flags ->
         return $ flags{outputHard = Just str})
      ("path to collage without fading") :

   opt knead [] ["output-shaped"]
      (flip ReqArg "PATH" $ \str flags ->
         return $ flags{outputShaped = Just str})
      ("path to generated collage") :

   opt knead [] ["output-shaped-hard"]
      (flip ReqArg "PATH" $ \str flags ->
         return $ flags{outputShapedHard = Just str})
      ("path to collage without fading") :

   opt generic [] ["output-overlap"]
      (flip ReqArg "FORMAT" $ \str flags ->
         return $ flags{outputOverlap = Just str})
      ("path format for overlapped pairs like '%s-%s-overlap.jpeg'") :

   opt generic [] ["output-distance-map"]
      (flip ReqArg "FORMAT" $ \str flags ->
         return $ flags{outputDistanceMap = Just str})
      ("path format for distance maps like '%s-distance.jpeg'") :

   opt knead [] ["output-shape"]
      (flip ReqArg "FORMAT" $ \str flags ->
         return $ flags{outputShape = Just str})
      ("path format for smooth part shape like '%s-shape-soft.jpeg'") :

   opt knead [] ["output-shape-hard"]
      (flip ReqArg "FORMAT" $ \str flags ->
         return $ flags{outputShapeHard = Just str})
      ("path format for hard part shape like '%s-shape-hard.jpeg'") :

   opt generic [] ["output-state"]
      (flip ReqArg "FORMAT" $ \str flags ->
         return $ flags{outputState = Just str})
      ("path format for program states like 'patch-state-%s.csv'") :

   opt generic [] ["quality"]
      (flip ReqArg "PERCENTAGE" $ \str flags ->
         fmap (\x -> flags{quality = x}) $
         parseNumber "compression quality" (\q -> 0<=q && q<=100) "a percentage" str)
      (printf "JPEG compression quality for output, default: %d"
         (quality defltOption)) :

   opt generic [] ["maximum-absolute-angle"] -- "max-abs-angle"
      (flip ReqArg "DEGREE" $ \str flags ->
         fmap (\x -> flags{maximumAbsoluteAngle = Degree x}) $
         parseNumber "maximum absolute angle" (0<=) "non-negative" str)
      (printf "Maximum absolute angle for test rotations, default: %f"
         (getDegree $ maximumAbsoluteAngle defltOption)) :

   opt generic [] ["number-angles"] -- "num-angles"
      (flip ReqArg "NATURAL" $ \str flags ->
         fmap (\x -> flags{numberAngleSteps = x}) $
         parseNumber "number of angle steps" (0<=) "non-negative" str)
      (printf "Number of steps for test rotations, default: %d"
         (numberAngleSteps defltOption)) :

   opt accelerate [] ["radon"]
      (NoArg $ \flags -> return $ flags{radonTransform = True})
      (printf "Use Radon transform for estimating orientation, default: disabled") :

   opt generic [] ["smooth"]
      (flip ReqArg "NATURAL" $ \str flags ->
         fmap (\x -> flags{smooth = x}) $
         parseNumber "smooth radius" (0<=) "non-negative" str)
      (printf "Smooth radius for DC elimination, default: %d"
         (smooth defltOption)) :

   opt generic [] ["pad-size"]
      (flip ReqArg "NATURAL" $ \str flags ->
         fmap (\x -> flags{padSize = x}) $
         parseNumber "pad size" (0<=) "non-negative" str)
      (printf "Pad size for matching convolution, default: %d"
         (padSize defltOption)) :

   opt generic [] ["minimum-overlap"]
      (flip ReqArg "FRACTION" $ \str flags ->
         fmap (\x -> flags{minimumOverlap = x}) $
         parseNumber "minimum overlap" (0<=) "non-negative" str)
      (printf "Minimum overlap portion between pairs of images, default: %f"
         (minimumOverlap defltOption)) :

   opt generic [] ["maximum-difference"]
      (flip ReqArg "FRACTION" $ \str flags ->
         fmap (\x -> flags{maximumDifference = x}) $
         parseNumber "maximum difference" (\x -> 0<=x && x<=1) "between 0 and 1" str)
      (printf "Maximum average difference between overlapping parts, default: %f"
         (maximumDifference defltOption)) :

   opt generic [] ["finetune-rotate"]
      (NoArg $ \flags -> return $ flags{finetuneRotate = True})
      (printf "Fine-tune rotation together with overlapping, default: disabled") :

   opt generic [] ["number-stamps"]
      (flip ReqArg "NATURAL" $ \str flags ->
         fmap (\x -> flags{numberStamps = x}) $
         parseNumber "number of stamps" (0<) "positive" str)
      (printf "Number of stamps in an overlap area, default: %d"
         (numberStamps defltOption)) :

   opt generic [] ["stamp-size"]
      (flip ReqArg "NATURAL" $ \str flags ->
         fmap (\x -> flags{stampSize = x}) $
         parseNumber "stamp size" (0<) "positive" str)
      (printf "Size of a stamp, default: %d"
         (stampSize defltOption)) :

   opt generic [] ["distance-gamma"]
      (flip ReqArg "FRACTION" $ \str flags ->
         fmap (\x -> flags{distanceGamma = x}) $
         parseNumber "gamma exponent" (0<) "positive" str)
      (printf "Distance exponent, default: %f"
         (distanceGamma defltOption)) :

   opt knead [] ["shape-smooth"]
      (flip ReqArg "NATURAL" $ \str flags ->
         fmap (\x -> flags{shapeSmooth = x}) $
         parseNumber "smooth radius" (0<=) "non-negative" str)
      (printf "Smooth radius for part shapes, default: %d"
         (shapeSmooth defltOption)) :

   []


description :: Description (Image, Args) -> EngineDescription (Image, Args)
description desc =
   map
      (mapSnd $ fmapOptDescr $ \update (image, old) -> do
         new <- update $ option old
         return (image, old {option = new}))
      (optionDescription desc)
   ++

   opt generic [] ["hint-angle"]
      (flip ReqArg "DEGREE" $ \str (image, args) ->
         fmap (\x -> (image{angle = Just (Degree x)}, args)) $
         parseNumber "angle" (\w -> -1000<=w && w<=1000) "degree" str)
      (printf "Angle of the next image in first phase, default: %s" $
       maybe "automatic estimation" (show . getDegree) (angle defltImage)) :

   []


addFile :: FilePath -> ((Image, Args) -> IO (Image, Args))
addFile path (image, args) =
   return (defltImage, args {inputs = (image,path) : inputs args})


get :: Engine -> IO Args
get engine = do
   let desc = map snd $ filter (EnumSet.get engine . fst) $ description desc
   argv <- Env.getArgs
   let (args, _files, errors) = getOpt (Opt.ReturnInOrder addFile) desc argv
   when (not $ null errors) $
      exitFailureMsg (init (concat errors))

   (lastImage, parsedArgs) <- foldl (>>=) (return (defltImage, defltArgs)) args

   when (lastImage /= defltImage) $
      exitFailureMsg "unused trailing image options"

   return parsedArgs

images ::
   (State.AngleCorrected angleCorr) => Args -> IO [State.Proposed angleCorr]
images args = do
   xs <- maybe (return Vector.empty) State.read (state $ option args)
   case Vector.toList xs ++ (reverse $ map proposedFromImage $ inputs args) of
      [] -> exitFailureMsg "no input files"
      imgs -> do
         let duplicates =
               Map.keys $
               Map.filter (ListHT.lengthAtLeast 2) $ Map.fromListWith (++) $
               map (\img -> (State.propPath img, [img])) imgs
         when (not $ null duplicates) $
            IO.hPutStrLn IO.stderr $
            "duplicate file paths: " ++ List.intercalate ", " duplicates
         return imgs
