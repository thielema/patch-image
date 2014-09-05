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
      quality :: Int
   }

defltOption :: Option
defltOption =
   Option {
      output = Nothing,
      outputHard = Nothing,
      quality = 99
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

   Opt.Option [] ["quality"]
      (flip ReqArg "PERCENTAGE" $ \str flags ->
         fmap (\x -> flags{quality = x}) $
         parseNumber "compression quality" (\q -> 0<=q && q<=100) "a percentage" str)
      (printf "JPEG compression quality for output, default: %d"
         (quality defltOption)) :

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
