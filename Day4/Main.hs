module Day4.Main where

import qualified Data.Foldable                 as F
import           Data.Function                  ( on )
import           Data.IntMap                    ( fromList )
import           Data.List
import           Data.Maybe
import qualified Data.Sequence                 as S
import qualified Data.Text                     as T

type Matrix a = S.Seq (S.Seq a)

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
          . map (S.fromList . map (\x -> (read x :: Int, False)) . words)
          $ drop 1 fileLines
  return (extraction, rows)

won :: S.Seq [(Int, Bool)] -> Bool
won = or . fmap (all ((== True) . snd))

searchInt x i = S.filter (\y -> fst y /= -1) $ fmap
  (foldl keepFound (-1 :: Int, -1 :: Int) . S.mapWithIndex mapToTuple)
  x
 where
  mapToTuple index value =
    (fromMaybe (-1) $ S.elemIndexL (i, False) value, index)
  keepFound a b = maximumBy (compare `on` fst) [a, b]

main :: IO ()
main = do
  contents <- parseFile
  let sip = searchInt (snd contents) 68
  print sip
