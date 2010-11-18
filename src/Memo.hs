module Memo where

import Data.Int

type MemoId = Int32
type MemoTime = String

data Memo = Memo {
  memoId :: MemoId,
  memoStr :: String,
  memoTime :: MemoTime
  }

idToInt :: MemoId -> Int
idToInt = fromIntegral

intToId :: Int -> MemoId
intToId = fromIntegral

showMemo :: Bool -> Memo -> String
showMemo True (Memo n s t) = show n ++ ":" ++ t ++ ":" ++ s
showMemo False (Memo n s t) = take 8 (drop 11 t) ++ ":" ++ s

readMemo :: String -> Memo
readMemo m = Memo (read p1) p3 p2 where
  (p1, rest) = break (== ':') m
  (p2, p3) = break (== ':') rest
