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
        runInteractiveProcess "ssh" ["-t", "memoplex", "-ir"] Nothing Nothing
      -- check for eof/errors?
      forkIO . forever $ do
        memo <- readMemo <$> hGetLine pOut
        insertMemo conn (memoId memo) (memoStr memo)
        atomically . writeTChan mChan . show $ memoId memo
      return pIn
    mainLoop conn rSock wSock rChan wChan [] [] mChan pIns) `finally` 
      (sClose rSock >> sClose wSock)

acceptLoop :: Socket -> TChan Client -> IO ()
acceptLoop sock chan = forever $ do
  (cHandle, host, port) <- accept sock
  cChan <- atomically newTChan
  cTID <- forkIO $ clientLoop cHandle cChan
  atomically $ writeTChan chan (cChan, cHandle)

ioErrCloseNoth :: Handle -> IOException -> IO (Maybe a)
ioErrCloseNoth h _ = hClose h >> return Nothing

clientLoop :: Handle -> TChan String -> IO ()
clientLoop h chan =
  listenLoop (hGetLine h) chan
    `catch` (\ (_ :: IOException) -> return ())
    `finally` hClose h

listenLoop :: IO a -> TChan a -> IO ()
listenLoop act chan = forever (act >>= atomically . writeTChan chan)

flushClis :: [Client] -> IO [Client]
flushClis clis = fmap catMaybes . forM clis $ \ c@(_, h) -> do
  hFlush h
  return $ Just c
  `catch` ioErrCloseNoth h

tellClis :: [Client] -> String -> IO [Client]
tellClis clis s = fmap catMaybes . forM clis $ \ c@(_, h) -> do
  hPutStrLn h s
  hFlush h
  return $ Just c
  `catch` ioErrCloseNoth h

mainLoop :: Connection -> Socket -> Socket -> TChan Client -> TChan Client -> 
  [Client] -> [Client] -> TChan String -> [Handle] -> IO ()
mainLoop conn rSock wSock rChan wChan rClis wClis mChan pIns = select [
  Sel (readTChan rChan) $ \ c ->
    --putStrLn "reader" >>
    mainLoop conn rSock wSock rChan wChan (c:rClis) wClis mChan pIns,
  Sel (readTChan wChan) $ \ c -> 
    --putStrLn "writer" >>
    mainLoop conn rSock wSock rChan wChan rClis (c:wClis) mChan pIns,
  Sel (tselect rClis) $ \ (s, _) -> do
    --putStrLn "read"
    markMemosSeen conn [read s]
    mapM_ (\ h -> hPutStrLn h "" >> hFlush h) pIns
    mainLoop conn rSock wSock rChan wChan rClis wClis mChan pIns,
  Sel (tselect wClis) $ \ (s, _) -> do
    --putStrLn "write"
    wClis' <- flushClis wClis
    rClis' <- tellClis rClis s
    mainLoop conn rSock wSock rChan wChan rClis' wClis' mChan pIns,
  Sel (readTChan mChan) $ \ s -> do
    wClis' <- flushClis wClis
    rClis' <- tellClis rClis s
    mainLoop conn rSock wSock rChan wChan rClis' wClis' mChan pIns
  ]

