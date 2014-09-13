module Option where

import Option.Utility (exitFailureMsg, parseNumber, fmapOptDescr)

import qualified System.Console.GetOpt as Opt
import qualified System.Environment as Env
import System.Console.GetOpt (ArgDescr(NoArg, ReqArg), getOpt, usageInfo)

import qualified System.Exit as Exit

import Control.Monad (when)

import qualified Distribution.Verbosity as Verbosity
import qualified Distribution.ReadE as ReadE
import Distribution.Verbosity (Verbosity)

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
      output :: Maybe FilePath,
      outputHard :: Maybe FilePath,
      outputOverlap :: Maybe String, -- e.g. "/tmp/%s-%s-overlap.jpeg"
      outputDistanceMap :: Maybe String, -- e.g. "/tmp/%s-distance.jpeg"
      quality :: Int,
      maximumAbsoluteAngle :: Float,
      numberAngleSteps :: Int,
      smooth :: Int,
      minimumOverlap :: Float,
      maximumDifference :: Float,
      finetuneRotate :: Bool,
      numberStamps :: Int,
      stampSize :: Int,
      distanceGamma :: Float,
      padSize :: Int
   }

defltOption :: Option
defltOption =
   Option {
      verbosity = Verbosity.verbose,
      output = Nothing,
      outputHard = Nothing,
      outputOverlap = Nothing,
      outputDistanceMap = Nothing,
      quality = 99,
      maximumAbsoluteAngle = 1,
      numberAngleSteps = 40,
      smooth = 20,
      minimumOverlap = 1/4,
      maximumDifference = 0.2,
      finetuneRotate = False,
      numberStamps = 5,
      stampSize = 64,
      distanceGamma = 2,
      padSize = 1024
   }


data Image =
   Image {
      angle :: Maybe Float
   }
   deriving (Eq)

defltImage :: Image
defltImage = Image {angle = Nothing}


type Description a = [Opt.OptDescr (a -> IO a)]

{-
Guide for common Linux/Unix command-line options:
  http://www.faqs.org/docs/artu/ch10s05.html
-}
optionDescription :: Description a -> Description Option
optionDescription desc =
   Opt.Option ['h'] ["help"]
      (NoArg $ \ _flags -> do
         programName <- Env.getProgName
         putStrLn $
            usageInfo
               ("Usage: " ++ programName ++
                " [OPTIONS]... [[INPUTOPTIONS]... INPUT]...") $
            desc
         Exit.exitSuccess)
      "show options" :

   Opt.Option ['v'] ["verbose"]
      (flip ReqArg "N" $ \str flags -> do
         case ReadE.runReadE Verbosity.flagToVerbosity str of
            Right n -> return (flags{verbosity = n})
            Left msg -> exitFailureMsg msg)
      (printf "verbosity level: 0..3, default: %d"
         (fromEnum $ verbosity defltOption)) :

   Opt.Option [] ["output"]
      (flip ReqArg "PATH" $ \str flags ->
         return $ flags{output = Just str})
      ("path to generated collage") :

   Opt.Option [] ["output-hard"]
      (flip ReqArg "PATH" $ \str flags ->
         return $ flags{outputHard = Just str})
      ("path to collage without fading") :

   Opt.Option [] ["output-overlap"]
      (flip ReqArg "FORMAT" $ \str flags ->
         return $ flags{outputOverlap = Just str})
      ("path format for overlapped pairs") :

   Opt.Option [] ["output-distance-map"]
      (flip ReqArg "FORMAT" $ \str flags ->
         return $ flags{outputDistanceMap = Just str})
      ("path format for distance maps") :

   Opt.Option [] ["quality"]
      (flip ReqArg "PERCENTAGE" $ \str flags ->
         fmap (\x -> flags{quality = x}) $
         parseNumber "compression quality" (\q -> 0<=q && q<=100) "a percentage" str)
      (printf "JPEG compression quality for output, default: %d"
         (quality defltOption)) :

   Opt.Option [] ["smooth"]
      (flip ReqArg "NATURAL" $ \str flags ->
         fmap (\x -> flags{smooth = x}) $
         parseNumber "smooth radius" (0<=) "non-negative" str)
      (printf "Smooth radius for DC elimination, default: %d"
         (smooth defltOption)) :

   Opt.Option [] ["maximum-absolute-angle"] -- "max-abs-angle"
      (flip ReqArg "DEGREE" $ \str flags ->
         fmap (\x -> flags{maximumAbsoluteAngle = x}) $
         parseNumber "maximum absolute angle" (0<=) "non-negative" str)
      (printf "Maximum absolute angle for test rotations, default: %f"
         (maximumAbsoluteAngle defltOption)) :

   Opt.Option [] ["number-angles"] -- "num-angles"
      (flip ReqArg "NATURAL" $ \str flags ->
         fmap (\x -> flags{numberAngleSteps = x}) $
         parseNumber "number of angle steps" (0<=) "non-negative" str)
      (printf "Number of steps for test rotations, default: %d"
         (numberAngleSteps defltOption)) :

   Opt.Option [] ["minimum-overlap"]
      (flip ReqArg "FRACTION" $ \str flags ->
         fmap (\x -> flags{minimumOverlap = x}) $
         parseNumber "minimum overlap" (0<=) "non-negative" str)
      (printf "Minimum overlap portion between pairs of images, default: %f"
         (minimumOverlap defltOption)) :

   Opt.Option [] ["maximum-difference"]
      (flip ReqArg "FRACTION" $ \str flags ->
         fmap (\x -> flags{maximumDifference = x}) $
         parseNumber "maximum difference" (\x -> 0<=x && x<=1) "between 0 and 1" str)
      (printf "Maximum average difference between overlapping parts, default: %f"
         (maximumDifference defltOption)) :

   Opt.Option [] ["finetune-rotate"]
      (NoArg $ \flags -> return $ flags{finetuneRotate = True})
      (printf "Fine-tune rotation together with overlapping, default: disabled") :

   Opt.Option [] ["number-stamps"]
      (flip ReqArg "NATURAL" $ \str flags ->
         fmap (\x -> flags{numberStamps = x}) $
         parseNumber "number of stamps" (0<) "positive" str)
      (printf "Number of stamps in an overlap area, default: %d"
         (numberStamps defltOption)) :

   Opt.Option [] ["stamp-size"]
      (flip ReqArg "NATURAL" $ \str flags ->
         fmap (\x -> flags{stampSize = x}) $
         parseNumber "stamp size" (0<) "positive" str)
      (printf "Size of a stamp, default: %d"
         (stampSize defltOption)) :

   Opt.Option [] ["distance-gamma"]
      (flip ReqArg "FRACTION" $ \str flags ->
         fmap (\x -> flags{distanceGamma = x}) $
         parseNumber "gamma exponent" (0<) "positive" str)
      (printf "Distance exponent, default: %f"
         (distanceGamma defltOption)) :

   Opt.Option [] ["pad-size"]
      (flip ReqArg "NATURAL" $ \str flags ->
         fmap (\x -> flags{padSize = x}) $
         parseNumber "pad size" (0<=) "non-negative" str)
      (printf "Pad size for matching convolution, default: %d"
         (padSize defltOption)) :

   []


description :: Description (Image, Args) -> Description (Image, Args)
description desc =
   map
      (fmapOptDescr $ \update (image, old) -> do
         new <- update $ option old
         return (image, old {option = new}))
      (optionDescription desc)
   ++

   Opt.Option [] ["hint-angle"]
      (flip ReqArg "DEGREE" $ \str (image, args) ->
         fmap (\x -> (image{angle = Just x}, args)) $
         parseNumber "angle" (\w -> -1000<=w && w<=1000) "degree" str)
      (printf "Angle of the next image in first phase, default: %s" $
       maybe "automatic estimation" show (angle defltImage)) :

   []


addFile :: FilePath -> ((Image, Args) -> IO (Image, Args))
addFile path (image, args) =
   return (defltImage, args {inputs = (image,path) : inputs args})



get :: IO Args
get = do
   let desc = description desc
   argv <- Env.getArgs
   let (args, _files, errors) = getOpt (Opt.ReturnInOrder addFile) desc argv
   when (not $ null errors) $
      exitFailureMsg (init (concat errors))

   (lastImage, parsedArgs) <- foldl (>>=) (return (defltImage, defltArgs)) args

   when (lastImage /= defltImage) $
      exitFailureMsg "unused trailing image options"

   case inputs parsedArgs of
      [] -> exitFailureMsg "no input files"
      images -> return $ parsedArgs {inputs = reverse images}
