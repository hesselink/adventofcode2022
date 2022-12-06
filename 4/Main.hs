module Main where

import Control.Applicative
import Data.Void
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

main :: IO ()
main = do
  f <- readFile "input/4"
  let rangePairs = parse f
      result = countFullyOverlapping rangePairs
  print result
  let result2 = countOverlapping rangePairs
  print result2

data Range = Range Int Int -- min/max
  deriving (Show, Eq)

parse :: String -> [(Range, Range)]
parse = runParser parseRangePairs

countFullyOverlapping :: [(Range, Range)] -> Int
countFullyOverlapping = length . filter (uncurry fullyOverlapping)

fullyOverlapping :: Range -> Range -> Bool
fullyOverlapping r1 r2 = fullyOverlapping1 r1 r2 || fullyOverlapping1 r2 r1

fullyOverlapping1 :: Range -> Range -> Bool
fullyOverlapping1 (Range l1 r1) (Range l2 r2) = l1 <= l2 && r1 >= r2

countOverlapping :: [(Range, Range)] -> Int
countOverlapping = length . filter (uncurry overlapping)

overlapping :: Range -> Range -> Bool
overlapping r1 r2 = overlapping1 r1 r2 || overlapping1 r2 r1

overlapping1 :: Range -> Range -> Bool
overlapping1 (Range l1 r1) (Range l2 _r2) = l1 <= l2 && r1 >= l2

runParser :: Parser a -> String -> a
runParser p = either (error . P.errorBundlePretty) id . P.runParser p "input"

type Parser = P.Parsec Void String

parseRangePairs :: Parser [(Range, Range)]
parseRangePairs = many (parseRangePair <* P.newline)

parseRangePair :: Parser (Range, Range)
parseRangePair = (,) <$> parseRange <* P.char ',' <*> parseRange

parseRange :: Parser Range
parseRange = Range <$> parseNumber <* P.char '-' <*> parseNumber

parseNumber :: Parser Int
parseNumber = read <$> many P.numberChar
