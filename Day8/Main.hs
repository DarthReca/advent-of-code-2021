module Day8.Main where
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T

parseFile :: IO [([T.Text], [T.Text])]
parseFile = do
  contents <- readFile "input.txt"
  let patternsOutput =
        map (listToTuple . map T.words . T.splitOn (T.pack "|") . T.pack)
          $ lines contents
  return patternsOutput

listToTuple :: [a] -> (a, a)
listToTuple [a, b] = (a, b)

main :: IO ()
main = do
  contents <- parseFile
  let mapped = Map.fromListWith (+)
        $ concatMap (\(p, o) -> map (\x -> (T.length x, 1)) o) contents
  let firstCount = Map.foldl (+) 0
        $ Map.filterWithKey (\k v -> k `elem` [2, 4, 7, 3]) mapped
  print firstCount
