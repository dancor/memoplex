{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Int
import Data.List
import Data.Maybe
import Database.Enumerator
import Database.PostgreSQL.Enumerator
import Network
import Serve
import System.Environment
import System.Console.GetOpt
import System.IO
import System.Random

type MemoId = Int32
type MemoTime = String

data MemoMode = MemoRead | MemoWrite | MemoServe

data Memo = Memo MemoId String MemoTime

data Options = Options {
  optHelp :: Bool,
  optMode :: Maybe MemoMode
  }

instance Applicative (DBM mark sess) where
  pure = return
  (<*>) = ap

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

idToInt :: MemoId -> Int
idToInt = fromIntegral

intToId :: Int -> MemoId
intToId = fromIntegral

oneIdIteratee :: (Monad m) => Int -> IterAct m MemoId
oneIdIteratee = const . return . Left . intToId

--insertMemo :: MemoId -> String -> IO Int
insertMemo :: 
  (Statement stmt sess q,
  QueryIteratee (DBM mark sess) q i Int ColumnBuffer,
  IQuery q sess ColumnBuffer) =>
  MemoId -> String -> DBM mark sess MemoId
insertMemo n s = doQuery (sqlbind
  "INSERT INTO memoplex (memo_id, memo_text) VALUES (?, ?) RETURNING memo_id"
  [bindP $ idToInt n, bindP s])
  oneIdIteratee undefined
  <* commit

memoIteratee :: (Monad m) => Int -> String -> MemoTime ->
  IterAct m [Memo]
memoIteratee i s t accum = result' $ Memo (intToId i) s t : accum

--readPrevMemos :: IO [Memo]
readPrevMemos = doQuery (sql
  "SELECT memo_id, memo_text, memo_time FROM memoplex \
  \WHERE seen = FALSE ORDER BY memo_time DESC")
  memoIteratee []
  <* commit

--readMemo n :: MemoId -> Maybe Memo
readMemo n = listToMaybe <$> doQuery (sqlbind
  "SELECT memo_text, memo_time FROM memoplex \
  \WHERE memo_id = ? AND seen = FALSE"
  [bindP $ idToInt n])
  (memoIteratee $ idToInt n) []
  <* commit

markMemosSeen [] = return 0
markMemosSeen ns = execDML (cmdbind
  ("UPDATE memoplex SET seen = TRUE WHERE memo_id in (" ++
    intercalate ", " ["?" | _ <- ns] ++ ")")
  (map (bindP . idToInt) ns))
  <* commit

io :: (MonadIO m) => IO a -> m a
io = liftIO

showMemo :: Memo -> String
showMemo (Memo n s t) = show n ++ ":" ++ t ++ ":" ++ s

memoplex :: MemoMode -> IO ()
memoplex mode = withSocketsDo . withSession (connect [CAdbname "me_log"]) $ 
  case mode of
    MemoRead -> do
      h <- io $ connectTo "localhost" (PortNumber readerNotifyPort)
      --memos <- readPrevMemos
      forever $ do
        n <- io $ read <$> hGetLine h
        m <- readMemo n
        maybe (return ()) (io . putStrLn . showMemo) m
    MemoWrite -> do
      h <- io $ connectTo "localhost" (PortNumber writerNotifyPort)
      ls <- io $ lines <$> getContents
      flip mapM_ ls $ \ l -> do
        n <- io $ intToId <$> randomIO
        insertMemo n l
        io . hPutStrLn h $ show n
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
