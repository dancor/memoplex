module NotifOmg where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Data.List
import Data.Maybe
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Posix
import System.Process
import qualified AnsiColor as C

-- System.Process is stupid right now so we have to do hackery
import System.Posix.Signals

memo :: Chan (Maybe String) -> Handle -> IO ()
memo chan pOut = forever $ hGetLine pOut >>= writeChan chan . Just

pop :: FilePath -> Chan (Maybe String) -> IO ()
pop home chan = do
    let seenF = home </> ".config" </> "memoplex" </> "seen"
    doesFileExist seenF >>= flip when (removeFile seenF)
    createNamedPipe seenF $ unionFileModes ownerReadMode ownerWriteMode
    -- Must be ReadWriteMode even though we don't read due to strangeness
    -- with named pipes. Using a low level blocking openFile didn't work.
    -- But this does.
    h <- openFile seenF ReadWriteMode
    forever $ hGetLine h >> writeChan chan Nothing

getC :: [Char] -> [Char]
getC a = head . (++ [C.yellow]) . map snd $ filter (($ a) . fst) getCFs

getCFs :: [([Char] -> Bool, [Char])]
getCFs =
    [ (("Mail:gen:" `isPrefixOf`), C.cyan)
    , (("Mail:goo:" `isPrefixOf`), C.magenta)
    , (("aim:" `isPrefixOf`), C.white)
    ]

windowAct :: Handle -> Bool -> Maybe String -> IO Bool
windowAct pIn alreadyMarked Nothing = do
    hPutStrLn pIn ""
    hFlush pIn
    unless alreadyMarked . putStrLn $
        C.green ++ "-----------------------------"
    system "wmctrl -r NOTIFIEROMG -b add,below || true"
    return True
windowAct _ _ (Just a) = do
    putStrLn $ getC a ++ a
    system "wmctrl -r NOTIFIEROMG -b add,above || true"
    return False

handler :: ThreadId -> ProcessHandle -> IO ()
handler mainThreadId pId = do
    terminateProcess pId
    throwTo mainThreadId (ExitFailure 1)

notifOmg :: String -> IO ()
notifOmg home = do
    hSetBuffering stdout LineBuffering
    chan <- newChan
    mainThreadId <- myThreadId
    -- using a separate process is historical;
    -- can switch to in-process?
    (pIn, pOut, pErr, pId) <-
      runInteractiveProcess "memoplex" ["-r"] Nothing Nothing
    installHandler sigINT (Catch $ handler mainThreadId pId) Nothing
    hSetBuffering pOut LineBuffering
    forkIO $ memo chan pOut
    forkIO $ pop home chan
    c <- getChanContents chan
    foldM_ (windowAct pIn) False c
