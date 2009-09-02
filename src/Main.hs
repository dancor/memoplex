module Main where

persistAtLeastSecs :: Int
persistAtLeastSecs = 5 * 60

main :: IO ()
main = do
  putStrLn "hi"
