{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Maybe
import Database.PostgreSQL.Enumerator
import Network
import Serve
import System.Environment
import System.Console.GetOpt
import System.IO

-- idk why this is needed?
instance Applicative (DBM mark sess) where
  pure = return
  (<*>) = ap

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

oneIntIteratee :: (Monad m) => Int -> IterAct m Int
oneIntIteratee = const . return . Left

--insertMemo :: String -> IO Int
insertMemo s = doQuery (sqlbind
  "INSERT INTO memoplex (memo) VALUES (?) RETURNING num"
  [bindP s])
  oneIntIteratee undefined
  <* commit

intStrDIteratee :: (Monad m) => Int -> String -> String ->
  IterAct m [(Int, (String, String))]
intStrDIteratee a b c accum = result' $ (a, (b, c)) : accum

--readNewMemos :: Int -> IO [(Int, String)]
readNewMemos (n :: Int) = doQuery (sqlbind
  "SELECT num, memo, time FROM memoplex WHERE num > ? AND \
  \time > now() - interval '5 minutes' ORDER BY num DESC"
  [bindP n])
  intStrDIteratee []
  <* commit

markMemosSeen [] = return 0
markMemosSeen ns = execDML (cmdbind
  ("UPDATE memoplex SET seen = TRUE WHERE num in (" ++
    intercalate ", " ["?" | _ <- ns] ++ ")")
  (map bindP ns))
  <* commit

io :: (MonadIO m) => IO a -> m a
io = liftIO

--memoReadMore :: Int -> IO ()
memoRead h lastMemoNum = do
  memos <- readNewMemos lastMemoNum
  let
    -- ghetto date string manipulation
    showMemo (s, time) = s ++ " " ++ take 8 (drop 11 time)
    seenNums = map fst memos
    lastMemoNum' = maximum (lastMemoNum:seenNums)
  io $ mapM_ (putStrLn . showMemo . snd) memos
  io . when (null memos) $ putStrLn "."
  markMemosSeen seenNums
  io $ hGetLine h
  memoRead h lastMemoNum'

memoplex :: MemoMode -> IO ()
memoplex mode = withSocketsDo $
  withSession (connect [CAdbname "me_log"]) $ case mode of
    MemoRead -> do
      h <- io $ connectTo "localhost" (PortNumber readerNotifyPort)
      memoRead h 0
    MemoWrite -> do
      h <- io $ connectTo "localhost" (PortNumber writerNotifyPort)
      ls <- io $ lines <$> getContents
      flip mapM_ ls $ \ l -> do
        insertMemo l
        io $ hPutStrLn h "."
        io $ hFlush h
    MemoServe -> io serveMain

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
