module Day3.Main where

import Data.Char
import Data.List

parseFile :: IO [String]
parseFile = do
    contents <- readFile "input.txt"
    let fileWords = map (takeWhile (\x -> x /= '\r')) $ lines contents
    return fileWords

groupByPosition :: [String] -> [[Int]]
groupByPosition fileLines = transpose $ map (\x -> lineToInts x) fileLines
  where lineToInts line = map digitToInt line

countOccurences :: [Int] -> (Int, Int)
countOccurences list = foldl (increase) (0,0) list
  where 
    increase x 1 = (fst x, snd x + 1)
    increase x 0 = (fst x + 1, snd x)

tupleToEpsilonBit :: (Int, Int) -> Int
tupleToEpsilonBit tup
  | fst tup > snd tup = 0
  | otherwise = 1

tupleToGammaBit :: (Int, Int) -> Int
tupleToGammaBit tup
  | fst tup < snd tup = 0
  | otherwise = 1

binToDec :: Int -> Int
binToDec 0 = 0
binToDec i = 2 * binToDec (div i 10) + (mod i 10)

listToInt :: [Int] -> Int
listToInt i = read $ concatMap show i 

main = do
  lin <- parseFile
  let bitCount = map countOccurences $ groupByPosition lin
  let gamma = binToDec. listToInt $ map tupleToGammaBit bitCount
  let epsilon = binToDec . listToInt $ map tupleToEpsilonBit bitCount
  print $ gamma * epsilon

