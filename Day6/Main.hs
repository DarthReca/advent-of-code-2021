module Day6.Main where

import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T

parseFile :: IO (Map.Map Int Int)
parseFile = do
  contents <- readFile "input.txt"
  let ints =
        Map.fromListWith (+)
          . map (\x -> (read (T.unpack x) :: Int, 1))
          . T.splitOn (T.pack ",")
          $ T.pack contents
  return ints

addDay :: Map.Map Int Int -> Map.Map Int Int
addDay state = Map.unionWith
  (+)
  (Map.mapKeysWith (+) (\k -> if k == 0 then 6 else k - 1) state)
  (Map.singleton 8 $ Map.findWithDefault 0 0 state)

main :: IO ()
main = do
  state <- parseFile
  let newState n = iterate addDay state !! n
  let livefor n = Map.foldl (+) 0 $ newState n
  print $ livefor 18
  -- part 2
  print $ livefor 256
