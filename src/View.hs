module Main where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Data.List
import Data.Maybe
import System.Exit
import System.IO
import System.Process
import qualified AnsiColor as C

-- System.Process is stupid right now so we have to do hackery
import System.Posix.Signals

memo :: Chan (Maybe String) -> Handle -> IO ()
memo chan pOut = forever $ hGetLine pOut >>= writeChan chan . Just

pop :: Chan (Maybe String) -> IO ()
pop chan = forever $ do
  -- could do this without external process
  -- and make it not danl specific lol
  readProcess "cat" ["/home/danl/log/notifyDelim"] ""
  writeChan chan Nothing

getC :: [Char] -> [Char]
getC a = head . (++ [C.yellow]) . map snd $ filter (($ a) . fst) getCFs

getCFs :: [([Char] -> Bool, [Char])]
getCFs = [
  (("Mail:gen:" `isPrefixOf`), C.cyan),
  (("Mail:goo:" `isPrefixOf`), C.magenta),
  (("aim:" `isPrefixOf`), C.white)
  ]

windowAct :: Bool -> Maybe String -> IO Bool
windowAct alreadyMarked Nothing = do
  unless alreadyMarked . putStrLn $ C.green ++ "-----------------------------"
  system "wmctrl -r NOTIFIEROMG -b add,below || true"
  return True
windowAct _ (Just a) = do
  putStrLn $ getC a ++ a
  system "wmctrl -r NOTIFIEROMG -b add,above || true"
  return False

handler :: ThreadId -> ProcessHandle -> IO ()
handler mainThreadId pId = do
  terminateProcess pId
  throwTo mainThreadId (ExitFailure 1)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  chan <- newChan
  mainThreadId <- myThreadId
  (pIn, pOut, pErr, pId) <-
    runInteractiveProcess "memoplex" ["-r"] Nothing Nothing
  installHandler sigINT (Catch $ handler mainThreadId pId) Nothing
  hClose pIn
  hSetBuffering pOut LineBuffering
  forkIO $ memo chan pOut
  forkIO $ pop chan
  c <- getChanContents chan
  foldM_ windowAct False c
