module Db where

import Control.Applicative
import Database.HDBC
import Database.HDBC.PostgreSQL
import Data.Maybe
import Data.List

import Memo

insertMemo :: Connection -> MemoId -> String -> IO Integer
insertMemo c n s = withTransaction c $ \ t -> run t
  "INSERT INTO memoplex (memo_id, memo_text) VALUES (?, ?) RETURNING memo_id"
  [toSql $ idToInt n, toSql s]

readPrevMemos :: Connection -> IO [Memo]
readPrevMemos c =
  map (\ [n, s, t] -> Memo (fromSql n) (fromSql s) (fromSql t)) <$> 
  withTransaction c (\ t -> quickQuery t
    "SELECT memo_id, memo_text, memo_time FROM memoplex \
    \WHERE memo_seen = FALSE ORDER BY memo_time DESC"
    [])

readMemo :: Connection -> MemoId -> IO (Maybe Memo)
readMemo c n = 
  listToMaybe . map (\ [s, t] -> Memo n (fromSql s) (fromSql t)) <$> 
  withTransaction c (\ t -> quickQuery t
    "SELECT memo_text, memo_time FROM memoplex \
    \WHERE memo_id = ? AND memo_seen = FALSE"
    [toSql $ idToInt n])

markMemosSeen :: Connection -> [MemoId] -> IO Integer
markMemosSeen c [] = return 0
markMemosSeen c ns = withTransaction c $ \ t -> run t
  ("UPDATE memoplex SET memo_seen = TRUE WHERE memo_id in (" ++
    intercalate ", " ["?" | _ <- ns] ++ ")")
  (map (toSql . idToInt) ns)

dbConn :: IO Connection
dbConn = connectPostgreSQL "dbname=me_log"

