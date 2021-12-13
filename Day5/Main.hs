{-# LANGUAGE TupleSections #-}
module Day5.Main where

import           Control.Exception              ( NoMethodError(NoMethodError) )
import           Data.Char                      ( digitToInt )
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T

parseFile :: IO [[(Int, Int)]]
parseFile = do
  contents <- readFile "input.txt"
  let fileWords =
        map (map coordinateToTuple . T.splitOn (T.pack "->") . T.pack)
          $ lines contents
  return fileWords
 where
  coordinateToTuple coord =
    listToTuple . map (\x -> read (T.unpack x) :: Int) $ T.splitOn
      (T.pack ",")
      coord

listToTuple :: [a] -> (a, a)
listToTuple [a, b] = (a, b)

expandXandY (x1, y1) (x2, y2)
  | x1 == x2  = map (\y -> ((x1, y), 1)) (listOfInt y1 y2)
  | otherwise = map (\x -> ((x, y1), 1)) (listOfInt x1 x2)
  where listOfInt a1 a2 = if a1 > a2 then [a1, a1 - 1 .. a2] else [a1 .. a2]

expandDiagonal (x1, y1) (x2, y2)
  | x1 < x2 && y1 < y2 = zipWith (curry (, 1)) [x1 .. x2] [y1 .. y2]
  | x1 < x2 && y1 > y2 = zipWith (curry (, 1)) [x1 .. x2] [y1, y1 - 1 .. y2]
  | x1 > x2 && y1 < y2 = zipWith (curry (, 1)) [x1, x1 - 1 .. x2] [y1 .. y2]
  | otherwise = zipWith (curry (, 1)) [x1, x1 - 1 .. x2] [y1, y1 - 1 .. y2]

sameXorY :: (Int, Int) -> (Int, Int) -> Bool
sameXorY (a1, b1) (a2, b2) = a1 == a2 || b1 == b2

main :: IO ()
main = do
  contents <- parseFile
  let filtered = filter (\x -> sameXorY (head x) (last x)) contents
  let expanded = concatMap (\x -> expandXandY (head x) (last x)) filtered
  let mapped   = Map.fromListWith (+) expanded
  print . Map.size $ Map.filter (> 1) mapped
  -- Part 2
  let part2Filtered = filter (\x -> not $ sameXorY (head x) (last x)) contents
  let expandDiag =
        concatMap (\x -> expandDiagonal (head x) (last x)) part2Filtered
  let unionMap = Map.unionWith (+) mapped $ Map.fromListWith (+) expandDiag
  print . Map.size $ Map.filter (> 1) unionMap
