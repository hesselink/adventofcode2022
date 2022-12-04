module Main where

import Data.List
import Data.List.Split
import Data.Char

main :: IO ()
main = do
  f <- readFile "input/3"
  let rucksacks = parseRucksacks f
      result = scoreDuplicates rucksacks
  print result
  let groups = parseGroups f
      result2 = scoreGroups groups
  print result2

scoreDuplicates :: [Rucksack] -> Int
scoreDuplicates = sum . map priority . map commonItem

scoreGroups :: [Group] -> Int
scoreGroups = sum . map priority . map commonItem3

type Rucksack = (Compartment, Compartment)
type Compartment = [Item]
type Item = Char

type Group = [String]

parseRucksacks :: String -> [Rucksack]
parseRucksacks = map parseRucksack . lines

parseRucksack :: String -> Rucksack
parseRucksack str = splitAt (length str `div` 2) str

parseGroups :: String -> [Group]
parseGroups = chunksOf 3 . lines

commonItem :: Rucksack -> Item
commonItem (c1, c2) = head $ intersect c1 c2

commonItem3 :: Group -> Item
commonItem3 [r1, r2, r3] = head $ intersect r1 (intersect r2 r3)
commonItem3 _ = error "Group without 3 members"

priority :: Item -> Int
priority i =
  let o = ord i
  in if o < 91
     then o - 38
     else o - 96
