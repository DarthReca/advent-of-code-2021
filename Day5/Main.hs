module Day5.Main where

import           Data.Char                      ( digitToInt )
import qualified Data.Text                     as T

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

listToTuple :: [a] -> (a, a, Int)
listToTuple [a, b] = (a, b, 1)

expandCouple :: [(Int, Int, Int)] -> [(Int, Int, Int)]
expandCouple couple = [(fst $ head couple, 1, 1)]

main :: IO ()
main = do
  contents <- parseFile
  print contents
