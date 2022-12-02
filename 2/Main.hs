{-# LANGUAGE LambdaCase #-}
module Main where

main :: IO ()
main = do
  f <- readFile "input/2"
  let moves = parse f
      res = totalScore moves
  print res
  let moves2 = parse2 f
      res2 = totalScore2 moves2
  print res2

totalScore :: [Round] -> Integer
totalScore = sum . map score

totalScore2 :: [Round2] -> Integer
totalScore2 = sum . map score2

parse :: String -> [Round]
parse = map parseRound . lines

parse2 :: String -> [Round2]
parse2 = map parseRound2 . lines

data Round = Round
  { opponent :: Play
  , you :: Play
  } deriving (Show, Eq)

parseRound :: String -> Round
parseRound (o:' ':y:[]) = Round (parsePlay o) (parsePlay y)
parseRound str = error $ "Unable to parse round: " ++ str

data Play = Rock | Paper | Scissors deriving (Eq, Show)

parsePlay :: Char -> Play
parsePlay = \case
  'A' -> Rock
  'B' -> Paper
  'C' -> Scissors
  'X' -> Rock
  'Y' -> Paper
  'Z' -> Scissors
  c -> error $ "Unknown play: " ++ show c

data Result = Lose | Draw | Win deriving (Eq, Show)

result :: Round -> Result
result = \case
  (Round Rock Scissors) -> Lose
  (Round Paper Rock) -> Lose
  (Round Scissors Paper) -> Lose
  (Round o y) -> if o == y then Draw else Win

score :: Round -> Integer
score r = scorePlay (you r) + scoreResult (result r)

scorePlay :: Play -> Integer
scorePlay = \case
  Rock -> 1
  Paper -> 2
  Scissors -> 3

scoreResult :: Result -> Integer
scoreResult = \case
  Lose -> 0
  Draw -> 3
  Win -> 6

parseResult :: Char -> Result
parseResult = \case
  'X' -> Lose
  'Y' -> Draw
  'Z' -> Win
  c -> error $ "Unknown result: " ++ show c

data Round2 = Round2
  { opponent2 :: Play
  , neededResult :: Result
  } deriving (Show, Eq)

parseRound2 :: String -> Round2
parseRound2 (o:' ':r:[]) = Round2 (parsePlay o) (parseResult r)
parseRound2 str = error $ "Unable to parse round2: " ++ str

neededPlay :: Play -> Result -> Play
neededPlay m = \case
  Draw -> m
  Lose -> case m of
    Rock -> Scissors
    Paper -> Rock
    Scissors -> Paper
  Win -> case m of
    Rock -> Paper
    Paper -> Scissors
    Scissors -> Rock

score2 :: Round2 -> Integer
score2 (Round2 o r) =
  let y = neededPlay o r
  in score (Round o y)
