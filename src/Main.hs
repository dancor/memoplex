{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Database.HDBC
import Network
import Prelude hiding (catch)
import Serve
import System.Environment
import System.FilePath
import System.Console.GetOpt
import System.IO
import System.Process
import System.Random

import Db
import Memo
import NotifOmg
import Opts

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
      Just mode -> memoplex opts mode

memoplex :: Options -> MemoMode -> IO ()
memoplex opts mode = do
  home <- getEnv "HOME"
  let
    dir = home </> ".memoplex"
    readF = dir </> "read"
    writeF = dir </> "write"
    reHVar hVar = threadDelay 500000 >> catch 
      (modifyMVar_ hVar $ \ _ -> connectTo "localhost" (UnixSocket readF))
      (\ (_ :: IOException) -> reHVar hVar)
    tryHGet :: MVar Handle -> IO String
    tryHGet hVar = catch 
      (readMVar hVar >>= hGetLine)
      (\ (_ :: IOException) -> reHVar hVar >> tryHGet hVar)
    tryHPut hVar s = catch 
      (readMVar hVar >>= \ h -> hPutStrLn h s >> hFlush h)
      (\ (_ :: IOException) -> reHVar hVar >> tryHPut hVar s)
  case mode of
    MemoRead -> handleSqlError $ do
      c <- dbConn
      h <- connectTo "localhost" (UnixSocket readF)
      hVar <- newMVar h
      memos <- readPrevMemos c
      mapM_ (putStrLn . showMemo (optShowIds opts)) memos
      loadedMemoIds <- newMVar $ map memoId memos
      let
        processMemo m = do
          putStrLn $ showMemo (optShowIds opts) m
          modifyMVar_ loadedMemoIds (return . (memoId m:))
      forkIO . forever $ do
        n <- read <$> tryHGet hVar
        memoMaybe <- getMemo c n
        maybe (return ()) processMemo memoMaybe
      forever $ getLine >> modifyMVar_ loadedMemoIds 
        ((>> return []) . mapM_ (tryHPut hVar . show))
    MemoWrite -> handleSqlError $ do
      c <- dbConn
      h <- connectTo "localhost" (UnixSocket writeF)
      ls <- lines <$> getContents
      forM_ ls $ \ l -> do
        n <- intToId <$> randomIO
        insertMemo c n l
        hPutStrLn h $ show n
        hFlush h
    MemoServe -> handleSqlError $ do
      c <- dbConn
      serveMain opts c dir readF writeF
    MemoNotifOmg -> notifOmg home
