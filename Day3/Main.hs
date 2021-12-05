module Day3.Main where

import Data.Char (digitToInt)
import Data.List (delete, transpose)

parseFile :: IO [String]
parseFile = do
  contents <- readFile "input.txt"
  let fileWords = map (takeWhile (/= '\r')) $ lines contents
  return fileWords

groupByPosition :: [String] -> [[Int]]
groupByPosition fileLines = transpose $ map lineToInts fileLines
  where
    lineToInts = map digitToInt

countOccurences :: [Int] -> (Int, Int)
countOccurences = foldl increase (0, 0)
  where
    increase x 1 = (fst x, snd x + 1)
    increase x 0 = (fst x + 1, snd x)
    increase x n = (0, 0)

tupleToBit :: (Int, Int) -> (Int -> Int -> Bool) -> Int
tupleToBit tup op
  | uncurry op tup = 0
  | otherwise = 1

selectBitSequence :: ([String], Int) -> (Int -> Int -> Bool) -> ([String], Int)
selectBitSequence x op = until (\x -> length (fst x) == 1) (\x -> (uncurry keep x . selectBit . countList $ fst x, snd x + 1)) x
  where
    countList = map countOccurences . groupByPosition
    selectBit = map (`tupleToBit` op)
    keep lin pos bit = filter (\x -> x !! pos == head (show (bit !! pos))) lin

main :: IO ()
main = do
  lin <- parseFile
  let bitCount = map countOccurences $ groupByPosition lin
  let gamma = concatMap (\x -> show $ tupleToBit x (<=)) bitCount
  let epsilon = concatMap (\x -> show $ tupleToBit x (>)) bitCount
  print $ "Gamma: " ++ show gamma ++ " Epsilon: " ++ show epsilon
  -- Part 2
  print $ selectBitSequence (lin, 0) (>) -- 3399
  print $ selectBitSequence (lin, 0) (<=) -- 1249
