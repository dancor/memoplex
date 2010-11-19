{-# LANGUAGE ScopedTypeVariables #-}

module Serve where
import Prelude hiding (catch)

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Database.HDBC.PostgreSQL
import Data.Maybe
import Network
import System.Directory
import System.IO
import System.Posix
import System.Process

import Db
import Memo
import Opts
import Select

type Client = (TChan String, Handle)

serveMain :: Options -> Connection -> FilePath -> FilePath -> FilePath -> IO ()
serveMain opts conn dir readF writeF = do
  createDirectoryIfMissing False dir
  doesFileExist readF >>= flip when (removeFile readF)
  doesFileExist writeF >>= flip when (removeFile writeF)
  -- wtf?
  --oldUmask <- setFileCreationMask (ownerReadMode `unionFileModes` ownerWriteMode)
  oldUmask <- setFileCreationMask 127
  rSock <- listenOn $ UnixSocket readF
  wSock <- listenOn $ UnixSocket writeF
  (do
    setFileCreationMask oldUmask
    wChan <- newTChanIO
    forkIO $ acceptLoop wSock wChan
    rChan <- newTChanIO
    forkIO $ acceptLoop rSock rChan
    mChan <- newTChanIO
    pIns <- forM (optPullHosts opts) $ \ host -> do
      (pIn, pOut, pErr, pId) <-                                    
        runInteractiveProcess "ssh" ["memoplex", "-ir"] Nothing Nothing
      -- check for eof/errors?
      forkIO . forever $ do
        memo <- readMemo <$> hGetLine pOut
        insertMemo conn (memoId memo) (memoStr memo)
        atomically . writeTChan mChan . show $ memoId memo
      return pIn
    mainLoop (State conn rSock wSock rChan wChan [] [] mChan pIns)) `finally` 
      (sClose rSock >> sClose wSock)

acceptLoop :: Socket -> TChan Client -> IO ()
acceptLoop sock chan = forever $ do
  (cHandle, host, port) <- accept sock
  cChan <- atomically newTChan
  cTID <- forkIO $ clientLoop cHandle cChan
  atomically $ writeTChan chan (cChan, cHandle)

clientLoop :: Handle -> TChan String -> IO ()
clientLoop h chan =
  listenLoop (hGetLine h) chan
    `catch` (\ (_ :: IOException) -> return ())
    `finally` hClose h

listenLoop :: IO a -> TChan a -> IO ()
listenLoop act chan = forever (act >>= atomically . writeTChan chan)

tellClis :: [Client] -> String -> IO ()
tellClis clis s = forM_ clis $ \ (_, h) -> forkIO $ do
  hPutStrLn h s
  hFlush h

data State = State {
  stConn :: Connection,
  stRSock :: Socket, 
  stWSock :: Socket,
  stRChan :: TChan Client,
  stWChan :: TChan Client,
  stRClis :: [Client],
  stWClis :: [Client],
  stMChan :: TChan String,
  stPIns :: [Handle]
  }

-- also prob want periodic closed reader/writer expiration
mainLoop :: State -> IO ()
mainLoop st = 
  putStrLn (show (length $ stRClis st) ++ " readers, " ++ 
    show (length $ stWClis st) ++ " writers") >> select [
  Sel (readTChan $ stRChan st) $ \ c -> do
    putStrLn "reader"
    mainLoop $ st {stRClis = c : stRClis st},
  Sel (readTChan $ stWChan st) $ \ c -> do
    putStrLn "writer"
    mainLoop $ st {stWClis = c : stWClis st},
  Sel (tselect $ stRClis st) $ \ (s, _) -> do
    putStrLn $ "read:" ++ s
    markMemosSeen (stConn st) [read s]
    mapM_ (\ h -> forkIO $ hPutStrLn h "" >> hFlush h) $ stPIns st
    mainLoop st,
  Sel (tselect $ stWClis st) $ \ (s, _) -> do
    putStrLn $ "write:" ++ s
    tellClis (stRClis st) s
    putStrLn $ "told-all"
    mainLoop st,
  Sel (readTChan $ stMChan st) $ \ s -> do
    putStrLn $ "memo:" ++ s
    tellClis (stRClis st) s
    mainLoop st
  ]

