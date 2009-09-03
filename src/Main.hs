module Main where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Network
import Serve
import System.Environment
import System.Console.GetOpt
import System.IO

data MemoMode = MemoRead | MemoWrite | MemoServe

data Options = Options {
  optHelp :: Bool,
  optMode :: Maybe MemoMode
}

defOpts :: Options
defOpts = Options {
  optHelp = False,
  optMode = Nothing
}

options :: [OptDescr (Options -> Options)]
options = [
  Option "h" ["help"] (NoArg (\ o -> o {optHelp = True})) "",
  Option "r" ["read"] (NoArg (\ o -> o {optMode = Just MemoRead}))
    "Read messages as they appear.",
  Option "s" ["serve"] (NoArg (\ o -> o {optMode = Just MemoServe}))
    "Connect writes to reads.",
  Option "w" ["write"] (NoArg (\ o -> o {optMode = Just MemoWrite}))
    "Accept new messages on stdin."
  ]

persistAtLeastSecs :: Int
persistAtLeastSecs = 5 * 60

memoplex :: MemoMode -> IO ()
memoplex mode = withSocketsDo $ do
  case mode of
    MemoRead -> do
      h <- connectTo "localhost" (PortNumber readerNotifyPort)
      -- todo: initial db stuff
      ls <- lines <$> hGetContents h
      flip mapM_ ls $ \ l -> do
        print "message."
    MemoWrite -> do
      h <- connectTo "localhost" (PortNumber writerNotifyPort)
      ls <- lines <$> getContents
      flip mapM_ ls $ \ l -> do
        -- todo: db stuff with l
        print $ "dealing with " ++ l
        hPutStrLn h "."
        hFlush h
    MemoServe -> serveMain

main :: IO ()
main = withSocketsDo $ do
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
    else case optMode opts of
      Nothing -> error $ showUsage "Must specify a mode of operation.\n"
      Just mode -> memoplex mode
