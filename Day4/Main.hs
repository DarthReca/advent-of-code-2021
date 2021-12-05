module Day4.Main where

import           Data.Foldable
import           Data.List
import qualified Data.Sequence                 as S
import qualified Data.Text                     as T
import           Data.Text.Read

parseFile = do
  contents <- readFile "input.txt"
  let fileLines = filter (not . null) $ lines contents
  let extraction =
        map (\x -> read (T.unpack x) :: Int)
          . T.splitOn (T.pack ",")
          . T.pack
          $ head fileLines
  let rows =
        S.chunksOf 5
          . S.fromList
          . map (map (\x -> read x :: Int) . words)
          $ drop 1 fileLines
  return (extraction, rows)

main :: IO ()
main = do
  contents <- parseFile
  print $ snd contents S.!? 0
