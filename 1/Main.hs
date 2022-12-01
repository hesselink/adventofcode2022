module Main where

import Data.List
import Data.List.Split

main :: IO ()
main = do
  f <- readFile "input/1"
  let parsed = parse f
      result = findMax parsed
  print result
  let result2 = findTop3 parsed
  print result2

parse :: String -> [[Integer]]
parse = map (map read) . splitOn [""] . lines

findMax :: [[Integer]] -> Integer
findMax = maximum . map sum

findTop3 :: [[Integer]] -> Integer
findTop3 = sum . take 3 . sortBy (flip compare) . map sum
