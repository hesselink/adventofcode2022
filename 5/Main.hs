module Main where

import Safe
import Data.Char
import Data.IntMap (IntMap)
import Data.List
import Data.List.Split
import qualified Data.IntMap as M
import qualified Text.Megaparsec.Char as P

import Lib.Parser

main :: IO ()
main = do
  f <- readFile "input/5"
  let initialState = parse f
      result = getMessage (execute initialState)
  putStrLn result

type Stack a = [a]

data State = State
  { stacks :: IntMap (Stack Char)
  , procedure :: [Step]
  } deriving (Show, Eq)

data Step = Step
  { amount :: Int
  , from :: Int
  , to :: Int
  } deriving (Show, Eq)

parse :: String -> State
parse str =
  case splitOn [""] . lines $  str of
    [sts, prc] -> State
      { stacks = parseStacks sts
      , procedure = parseProcedure prc
      }
    _ -> error "Incorrect input file"

parseStacks :: [String] -> IntMap (Stack Char)
parseStacks
  = M.fromList
  . map parseStack
  . filter (not . null)
  . map (reverse . filter isAlphaNum)
  . transpose

parseStack :: String -> (Int, Stack Char)
parseStack (n:st) = (read [n], reverse st)
parseStack _ = error "Empty stack"

parseProcedure :: [String] -> [Step]
parseProcedure = map (runParser parseStep)

parseStep :: Parser Step
parseStep = Step <$ P.string "move " <*> parseNumber <* P.string " from " <*> parseNumber <* P.string " to " <*> parseNumber

execute :: State -> State
execute currentState =
  if null (procedure currentState)
  then currentState
  else execute (step currentState)

step :: State -> State
step st =
  case procedure st of
    [] -> st
    (p:ps) -> State
      { stacks = doMove p (stacks st)
      , procedure = ps
      }

doMove :: Step -> IntMap (Stack Char) -> IntMap (Stack Char)
doMove st stacks =
  let fromStack = fromJustNote "stack not found" $ M.lookup (from st) stacks
      toStack = fromJustNote "stack not found" $ M.lookup (to st) stacks
      (items, newFromStack) = popMany (amount st) fromStack
      newToStack = pushMany items toStack
  in M.insert (from st) newFromStack (M.insert (to st) newToStack stacks)

getMessage :: State -> String
getMessage st = map (headNote "empty stack") $ M.elems (stacks st)

popMany :: Int -> Stack a -> ([a], Stack a)
popMany n st =
  let (is, st') = splitAt n st
  in (reverse is, st')

pushMany :: [a] -> Stack a -> Stack a
pushMany = (++)
