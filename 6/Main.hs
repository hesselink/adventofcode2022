{-# LANGUAGE LambdaCase #-}
module Main where

main :: IO ()
main = do
  f <- readFile "input/6"
  let result = detectStart f
  print result

detectStart :: String -> Int
detectStart = go 0
  where
    go n = \case
      (a:b:c:d:rs) -> if distinct [a,b,c,d] then (n+4) else go (n+1) (b:c:d:rs)
      _ -> error "no start found"

distinct :: (Show a, Eq a) => [a] -> Bool
distinct [] = True
distinct (x:xs) = not (x `elem` xs) && distinct xs
