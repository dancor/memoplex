{-# LANGUAGE ScopedTypeVariables #-}

module Serve where
import Prelude hiding (catch)

import Control.Applicative
import Control.Exception (finally, catch, IOException)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever, forM, filterM, liftM, when)
import Data.Maybe
import Network (listenOn, accept, sClose, Socket,
                withSocketsDo, PortID(..))
import System.IO
import System.Environment (getArgs)

writerNotifyPort = 1024
readerNotifyPort = 1025

type Client = (TChan (), Handle)

serveMain :: IO ()
serveMain = do
  writerNotifySocket <- listenOn $ PortNumber writerNotifyPort
  readerNotifySocket <- listenOn $ PortNumber readerNotifyPort
  serve writerNotifySocket readerNotifySocket
    `finally` (sClose writerNotifySocket >> sClose readerNotifySocket)

serve :: Socket -> Socket -> IO ()
serve writerNotifySocket readerNotifySocket = do
  acceptWriterChan <- atomically newTChan
  forkIO $ acceptLoop writerNotifySocket acceptWriterChan
  acceptReaderChan <- atomically newTChan
  forkIO $ acceptLoop readerNotifySocket acceptReaderChan
  putStrLn "serving"
  mainLoop writerNotifySocket readerNotifySocket acceptWriterChan
    acceptReaderChan[] []

acceptLoop :: Socket -> TChan Client -> IO ()
acceptLoop sock chan = forever $ do
  (cHandle, host, port) <- accept sock
  cChan <- atomically newTChan
  cTID <- forkIO $ clientLoop cHandle cChan
  atomically $ writeTChan chan (cChan, cHandle)

clientLoop :: Handle -> TChan () -> IO ()
clientLoop handle chan =
  listenLoop (hGetLine handle >> return ()) chan
    `catch` (\ (_ :: IOException) -> return ())
    `finally` hClose handle

listenLoop :: IO a -> TChan a -> IO ()
listenLoop act chan = sequence_ (repeat (act >>= atomically . writeTChan chan))

mainLoop :: Socket -> Socket -> TChan Client -> TChan Client -> [Client] ->
  [Client] -> IO ()
mainLoop writerNotifySocket readerNotifySocket acceptWriterChan
    acceptReaderChan writerClients readerClients = do
  r <- atomically $
    (Left . Left <$> readTChan acceptWriterChan) `orElse`
    (Left . Right <$> readTChan acceptReaderChan) `orElse`
    (Right <$> tselect writerClients)
  case r of
    Left (Left (ch, h)) -> do
      putStrLn "new writer"
      mainLoop writerNotifySocket readerNotifySocket acceptWriterChan
        acceptReaderChan ((ch, h) : writerClients) readerClients
    Left (Right (ch, h)) -> do
      putStrLn "new reader"
      mainLoop writerNotifySocket readerNotifySocket acceptWriterChan
        acceptReaderChan writerClients ((ch, h) : readerClients)
    Right (s, h) -> do
      putStrLn "write"
      print $ length writerClients
      print $ length readerClients
      writerClients' <- forM writerClients $ \ c@(_, h) -> do
        hFlush h
        return $ Just c
        `catch` (\ (_ :: IOException) -> hClose h >> return Nothing)
      readerClients' <- forM readerClients $ \ c@(_, h) -> do
        hPutStrLn h "."
        hFlush h
        return $ Just c
        `catch` (\ (_ :: IOException) -> hClose h >> return Nothing)
      let
        droppedWriters = length $ filter isNothing writerClients'
        droppedReaders = length $ filter isNothing readerClients'
      when (droppedWriters > 0) $
        putStrLn ("writers lost: " ++ show droppedWriters)
      when (droppedReaders > 0) $
        putStrLn ("readers lost: " ++ show droppedReaders)
      mainLoop writerNotifySocket readerNotifySocket acceptWriterChan
        acceptReaderChan (catMaybes writerClients') (catMaybes readerClients')

tselect :: [(TChan a, h)] -> STM (a, h)
tselect = foldl orElse retry .
  map (\ (ch, h) -> (flip (,) h) `fmap` readTChan ch)
