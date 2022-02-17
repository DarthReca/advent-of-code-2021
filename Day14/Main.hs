{-# LANGUAGE TupleSections #-}
module Day14.Main where

import           Data.List                      ( elemIndices
                                                , sortBy
                                                , sortOn
                                                )
import qualified Data.Map                      as M
import           Data.Maybe                     ( isJust )
import qualified Data.Text                     as T

parseFile :: IO (T.Text, M.Map T.Text T.Text)
parseFile = do
  contents <- readFile "input.txt"
  let lin      = map T.pack $ lines contents
  let template = head lin
  let insertionMap =
        M.fromList
          . map (listToTuple . map T.strip . T.splitOn (T.pack "->"))
          $ filter (isJust . T.find (== '-')) lin
  return (template, insertionMap)

listToTuple :: [a] -> (a, a)
listToTuple [a, b] = (a, b)

doStep :: T.Text -> M.Map T.Text T.Text -> T.Text
doStep template insertionMap = foldl
  (\acc a -> recomposeText (T.splitAt (fst a + 1) acc) (snd a))
  template
  updatePositions
 where
  insertingPosition t pat =
    map (, T.head $ insertionMap M.! pat) $ elemIndices pat t
  createCouples t =
    map (\(a, b) -> T.pack [a, b]) . T.zip t $ T.snoc (T.tail t) ' '
  insertingPositions = concatMap (insertingPosition (createCouples template))
    $ M.keys insertionMap
  updatePositions =
    zipWith (\a b -> (fst a + b, snd a)) (sortOn fst insertingPositions) [0 ..]
  recomposeText (a, b) c = T.append (T.snoc a c) b

main :: IO ()
main = do
  (template, insertionMap) <- parseFile
  print template
  print insertionMap
  let newTemplate = iterate (`doStep` insertionMap) template !! 40
  let counter =
        sortBy (\a b -> compare (snd a) (snd b))
          . M.toList
          . M.fromListWith (+)
          . map (, 1)
          $ T.unpack newTemplate
  print $ snd (last counter) - snd (head counter)
