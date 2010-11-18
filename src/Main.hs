{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans
import Database.HDBC
--import Data.List
import Network
import Serve
import System.Environment
import System.FilePath
import System.Console.GetOpt
import System.IO
import System.Random

import Db
import Memo
import NotifOmg

data MemoMode = MemoRead | MemoWrite | MemoServe | MemoNotifOmg

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
    "Accept new messages on stdin.",
  Option "N" ["notifieromg"] (NoArg (\ o -> o {optMode = Just MemoNotifOmg}))
    "Connect to custom terminal-based notifier."
  ]

memoplex :: MemoMode -> IO ()
memoplex mode = do
  home <- getEnv "HOME"
  let
    dir = home </> ".memoplex"
    readF = dir </> "read"
    writeF = dir </> "write"
  case mode of
    MemoRead -> handleSqlError $ do
      c <- dbConn
      h <- connectTo "localhost" $ UnixSocket readF
      memos <- readPrevMemos c
      mapM_ (putStrLn . showMemo) memos
      loadedMemoIds <- newMVar $ map memoId memos
      forkIO . forever $ do
        n <- read <$> hGetLine h
        memo <- readMemo c n
        maybe (return ()) (\ m -> putStrLn (showMemo m) >> 
          modifyMVar_ loadedMemoIds (return . (memoId m:))) memo
      forever $ getLine >> modifyMVar_ loadedMemoIds ((>> return []) . 
        mapM_ ((>> hFlush h) . hPutStrLn h . show))
    MemoWrite -> handleSqlError $ do
      c <- dbConn
      h <- connectTo "localhost" $ UnixSocket writeF
      ls <- lines <$> getContents
      forM_ ls $ \ l -> do
        n <- intToId <$> randomIO
        insertMemo c n l
        hPutStrLn h $ show n
        hFlush h
    MemoServe -> handleSqlError $ do
      c <- dbConn
      serveMain c dir readF writeF
    MemoNotifOmg -> notifOmg home

main :: IO ()
main = withSocketsDo $ do
  hSetBuffering stdout LineBuffering
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
