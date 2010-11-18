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

showMemo :: Memo -> String
showMemo (Memo n s t) = show n ++ ":" ++ t ++ ":" ++ s

