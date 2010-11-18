{-# LANGUAGE ExistentialQuantification #-}

module Select (Sel(..), select, tselect) where

import Control.Concurrent.STM

data Sel = forall a. Sel (STM a) (a -> IO ())
data ToDo = forall a. ToDo a (a -> IO ())

selToDo :: Sel -> STM ToDo
selToDo (Sel a f) = do
  x <- a
  return $ ToDo x f

select :: [Sel] -> IO ()
select ss = do
  ToDo x f <- atomically $ orElses $ map selToDo ss
  f x

orElses = foldl orElse retry

tselect :: [(TChan a, h)] -> STM (a, h)
tselect = orElses .
  map (\ (ch, h) -> (flip (,) h) `fmap` readTChan ch)
