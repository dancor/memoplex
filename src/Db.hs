{-# LANGUAGE Strict #-}

module Db where

import Control.Applicative
import Control.Monad
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Maybe
import Data.List
import System.Directory
import System.FilePath

import Memo

insertMemo :: Connection -> MemoId -> String -> IO ()
insertMemo c n s = withTransaction c $ \t -> do
    let nSql = toSql $ idToInt n
    run t "DELETE FROM memoplex WHERE memo_id = ?" [nSql]
    run t "INSERT INTO memoplex (memo_id, memo_text) VALUES (?, ?)"
        [nSql, toSql s]
    return ()

readPrevMemos :: Connection -> IO [Memo]
readPrevMemos c =
    map (\ [n, s, t] -> Memo (fromSql n) (fromSql s) (fromSql t)) <$> 
    withTransaction c (\ t -> quickQuery t
        "SELECT memo_id, memo_text, memo_time FROM memoplex \
        \WHERE memo_seen = 0 ORDER BY memo_time"
        [])

getMemo :: Connection -> MemoId -> IO (Maybe Memo)
getMemo c n = 
    listToMaybe . map (\ [s, t] -> Memo n (fromSql s) (fromSql t)) <$> 
    withTransaction c (\ t -> quickQuery t
        "SELECT memo_text, memo_time FROM memoplex \
        \WHERE memo_id = ? AND memo_seen = 0"
        [toSql $ idToInt n])

markMemosSeen :: Connection -> [MemoId] -> IO Integer
markMemosSeen c [] = return 0
markMemosSeen c ns = withTransaction c $ \ t -> run t
    ("UPDATE memoplex SET memo_seen = 1 WHERE memo_id in (" ++
        intercalate ", " ["?" | _ <- ns] ++ ")")
    (map (toSql . idToInt) ns)

dbConn :: FilePath -> IO Connection
dbConn confDir = do
    let sqlFile = confDir </> "me-log.db"
    dbExisted <- doesFileExist sqlFile
    createDirectoryIfMissing True confDir
    conn <- handleSqlError $ connectSqlite3 sqlFile
    unless dbExisted . withTransaction conn $ \c -> run c
        "CREATE TABLE memoplex (\
        \memo_id INTEGER PRIMARY KEY, \
        \memo_time TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
        \memo_text TEXT NOT NULL, \
        \memo_seen BOOL NOT NULL DEFAULT 0)" [] >> return ()
    return conn

withDbConn :: FilePath -> (Connection -> IO a) -> IO a
withDbConn confDir f = do
    c <- dbConn confDir
    ret <- f c
    disconnect c
    return ret
