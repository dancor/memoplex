module Memo where

import Data.Word

type MemoId = Word32
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

showMemo :: Memo -> String
showMemo (Memo n s t) = show n ++ ":" ++ t ++ ":" ++ s

