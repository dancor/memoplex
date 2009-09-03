module Main where

import Control.Monad
import Data.Maybe
import Network
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Posix.IO

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
  Option "w" ["write"] (NoArg (\ o -> o {optMode = Just MemoWrite}))
    "Accept new messages on stdin."
  ]

persistAtLeastSecs :: Int
persistAtLeastSecs = 5 * 60

memoplex :: MemoMode -> IO ()
memoplex mode = do
  home <- getHomeDirectory
  let
    memoDir = home </> ".memoplex"
    memoComm = memoDir </> "comm"
  createDirectoryIfMissing True memoDir
  case mode of
    MemoRead -> do
      doesFileExist memoComm >>= \ r -> unless r (writeFile memoComm "")
      fd <- openFd memoComm ReadOnly Nothing defaultFileFlags
      epoll <- create (fromJust $ toSize 10)
      evDesc <- add epoll () [inEvent] fd
      events <- wait (fromJust $ toDuration 10) epoll
      close epoll
      closeFd fd
      print $ length events
    MemoWrite -> do
      writeFile memoComm "."

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
