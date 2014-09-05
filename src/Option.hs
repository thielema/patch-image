module Option where

import Option.Utility (exitFailureMsg, parseNumber)

import qualified System.Console.GetOpt as Opt
import qualified System.Environment as Env
import System.Console.GetOpt (getOpt, usageInfo, ArgDescr(NoArg, ReqArg))

import qualified System.Exit as Exit

import Control.Monad (when)

import Text.Printf (printf)


data Args =
   Args {
      option :: Option,
      inputs :: [FilePath]
   }

data Option =
   Option {
      output :: Maybe FilePath,
      outputHard :: Maybe FilePath,
      outputOverlap :: Maybe String, -- e.g. "/tmp/%s-%s-overlap.jpeg"
      outputDistanceMap :: Maybe String, -- e.g. "/tmp/%s-distance.jpeg"
      quality :: Int,
      smooth :: Int,
      minimumOverlap :: Float,
      maximumDifference :: Float,
      padSize :: Int
   }

defltOption :: Option
defltOption =
   Option {
      output = Nothing,
      outputHard = Nothing,
      outputOverlap = Nothing,
      outputDistanceMap = Nothing,
      quality = 99,
      smooth = 20,
      minimumOverlap = 1/4,
      maximumDifference = 0.2,
      padSize = 1024
   }


type Description a = [Opt.OptDescr (a -> IO a)]

{-
Guide for common Linux/Unix command-line options:
  http://www.faqs.org/docs/artu/ch10s05.html
-}
description :: Description Option -> Description Option
description desc =
   Opt.Option ['h'] ["help"]
      (NoArg $ \ _flags -> do
         programName <- Env.getProgName
         putStrLn $
            usageInfo
               ("Usage: " ++ programName ++
                " [OPTIONS]... [INPUT]...") $
            desc
         Exit.exitSuccess)
      "show options" :

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

   Opt.Option [] ["pad-size"]
      (flip ReqArg "NATURAL" $ \str flags ->
         fmap (\x -> flags{padSize = x}) $
         parseNumber "pad size" (0<=) "non-negative" str)
      (printf "Pad size for matching convolution, default: %d"
         (padSize defltOption)) :

   []


get :: IO Args
get = do
   let desc = description desc
   argv <- Env.getArgs
   let (opts, files, errors) = getOpt Opt.RequireOrder desc argv
   when (not $ null errors) $
      exitFailureMsg (init (concat errors))

   parsedOpts <- foldl (>>=) (return defltOption) opts

   case files of
      [] -> exitFailureMsg "no input files"
      _ ->
         return $
         Args {
            option = parsedOpts,
            inputs = files
         }
