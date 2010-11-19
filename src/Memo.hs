module Memo where

import Control.Arrow
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

memoDelim = '|'

showMemo :: Bool -> Memo -> String
showMemo True (Memo n s t) = show n ++ [memoDelim] ++ t ++ [memoDelim] ++ s
showMemo False (Memo n s t) = take 8 (drop 11 t) ++ [memoDelim] ++ s

readMemo :: String -> Memo
readMemo m = Memo (read p1) p3 p2 where
  (p1, rest) = second tail $ break (== memoDelim) m
  (p2, p3) = second tail $ break (== memoDelim) rest
