module Day7.Main where

import qualified Data.Text                     as T

parseFile :: IO [Int]
parseFile = do
  contents <- readFile "input.txt"
  let ints =
        map (\x -> read (T.unpack x) :: Int) . T.splitOn (T.pack ",") $ T.pack
          contents
  return ints

constantFuelConsumption :: [Int] -> Int -> Int
constantFuelConsumption ints target =
  foldl (\a v -> a + abs (v - target)) 0 ints

linearFuelConsumption :: [Int] -> Int -> Int
linearFuelConsumption ints target =
  foldl (\a v -> a + sum [0 .. abs (v - target)]) 0 ints

main :: IO ()
main = do
  positions <- parseFile
  let possibilities = map (constantFuelConsumption positions)
                          [minimum positions .. maximum positions]
  print $ minimum possibilities
  -- Part 2
  let possibilities2 = map (linearFuelConsumption positions)
                           [minimum positions .. maximum positions]
  print $ minimum possibilities2

