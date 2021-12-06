module Day4.Main where

import           Data.Foldable
import           Data.List
import           Data.Maybe
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
          . map (map (\x -> (read x :: Int, False)) . words)
          $ drop 1 fileLines
  return (extraction, rows)

won :: S.Seq [(Int, Bool)] -> Bool
won = or . fmap (all ((== True) . snd))

searchInt x i = fmap (S.filter (isJust . elemIndex (i, False))) x

main :: IO ()
main = do
  contents <- parseFile
  let sip = searchInt (snd contents) 10
  print sip
  print . won . fromMaybe S.empty $ snd contents S.!? 0
