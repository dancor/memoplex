{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import Prelude hiding (catch)

import Network (listenOn, accept, sClose, Socket,
                withSocketsDo, PortID(..))
import System.IO
import System.Environment (getArgs)
import Control.Exception (finally, catch, IOException)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forM, filterM, liftM, when)

writerNotifyPort = 1024
readerNotifyPort = 1025

type Client = (TChan (), Handle)

main :: IO ()
main = withSocketsDo $ do
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
  mainLoop writerNotifySocket readerNotifySocket acceptChan [] []

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
mainLoop writerNotifySocket readerNotifySocket aceptWriterChan
    acceptReaderChan writerClients readerClients = do
  r <- atomically $
    (Left `fmap` readTChan acceptChan) `orElse`
    (Right `fmap` tselect clients)
  case r of
    Left (ch, h) -> do
      putStrLn "new client"
      mainLoop sock acceptChan $ (ch, h):clients
    Right (s, hSender) -> do
      putStrLn $ "data: " ++ s
      clients' <- forM clients $ \ c@(_, h) -> do
        when (h /= hSender) $ do
          hPutStrLn h "."
          hFlush h
        return [c]
        `catch` (\ (_ :: IOException) -> hClose h >> return [])
      let
        dropped = length $ filter null clients'
      when (dropped > 0) $ putStrLn ("clients lost: " ++ show dropped)
      mainLoop sock acceptChan $ concat clients'

tselect :: [(TChan a, h)] -> STM (a, h)
tselect = foldl orElse retry .
  map (\ (ch, h) -> (flip (,) h) `fmap` readTChan ch)
