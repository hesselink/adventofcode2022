{-# LANGUAGE LambdaCase #-}
module Main where

main :: IO ()
main = do
  f <- readFile "input/6"
  let result = detectStart 4 f
  print result
  let result2 = detectStart 14 f
  print result2

detectStart :: Int -> String -> Int
detectStart len = go 0
  where
    go n str = if distinct (take len str) then n + len else go (n+1) (tail str)

distinct :: (Show a, Eq a) => [a] -> Bool
distinct [] = True
distinct (x:xs) = not (x `elem` xs) && distinct xs
