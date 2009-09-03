module Main where

import Control.Monad
import Data.Maybe
import System.Environment
import System.Console.GetOpt

data ModeType = ModeRead | ModeWrite

data Options = Options {
  optHelp :: Bool,
  optMode :: Maybe ModeType
}

defOpts :: Options
defOpts = Options {
  optHelp = False,
  optMode = Nothing
}

options :: [OptDescr (Options -> Options)]
options = [
  Option "h" ["help"] (NoArg (\ o -> o {optHelp = True})) "",
  Option "r" ["read"] (NoArg (\ o -> o {optMode = Just ModeRead}))
    "Read messages as they appear.",
  Option "w" ["write"] (NoArg (\ o -> o {optMode = Just ModeWrite}))
    "Accept new messages on stdin."
  ]

persistAtLeastSecs :: Int
persistAtLeastSecs = 5 * 60

main :: IO ()
main = do
  argsOrig <- getArgs
  let
    usage = "usage:"
    showUsage = (++ usageInfo usage options)
    (opts, args) = case getOpt Permute options argsOrig of
      (o, n, []) -> (foldl (flip id) defOpts o, n)
      (_, _, errs) -> error . showUsage $ concat errs
  if optHelp opts
    then do
      progName <- getProgName
      putStr $ progName ++ ": " ++ showUsage ""
    else do
      when (isNothing $ optMode opts) . error $
        showUsage "Must specify a mode of operation.\n"
      putStrLn "hi"
