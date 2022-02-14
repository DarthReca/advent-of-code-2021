module Day12.Main where

import           Data.Foldable                  ( toList )
import           Data.Graph
import           Data.Maybe                     ( fromJust )
import qualified Data.Sequence                 as Seq
import qualified Data.Set                      as S
import qualified Data.Text                     as T

parseFile :: IO (S.Set T.Text, Graph)
parseFile = do
  contents <- readFile "input.txt"
  let stringToAdjTuple = listToTuple . T.splitOn (T.pack "-") . T.pack
  let textAdjacencyList = map stringToAdjTuple $ lines contents
  let nodes = S.fromList $ concatMap (\a -> [fst a, snd a]) textAdjacencyList
  let adjacencyList = concatMap
        (\(a, b) ->
          [ (S.findIndex a nodes, S.findIndex b nodes)
          , (S.findIndex b nodes, S.findIndex a nodes)
          ]
        )
        textAdjacencyList
  return (nodes, buildG (0, length nodes - 1) adjacencyList)

listToTuple :: [a] -> (a, a)
listToTuple [a, b] = (a, b)

allPathFromTo
  :: S.Set T.Text -> Graph -> Vertex -> Vertex -> Seq.Seq Integer -> Int
allPathFromTo nodes graph from to visitCounter
  | from == to
  = 1
  | not . any canVisit $ successors from
  = 0
  | otherwise
  = sum
    . map (\x -> allPathFromTo nodes graph x to (updateVisitCounter x))
    . filter canVisit
    $ successors from
 where
  updateVisitCounter index = Seq.adjust' (+ 1) index visitCounter
  canVisit v =
    T.toUpper (vertexName v)
      == vertexName v
      || fromJust (visitCounter Seq.!? v)
      <  1
  successors v = map snd . filter (\x -> fst x == v) $ edges graph
  vertexName v = S.elemAt v nodes


allPathFromTo2
  :: S.Set T.Text -> Graph -> Vertex -> Vertex -> Seq.Seq Integer -> Int
allPathFromTo2 nodes graph from to visitCounter
  | from == to
  = 1
  | not . any canVisit $ successors from
  = 0
  | otherwise
  = sum
    . map (\x -> allPathFromTo2 nodes graph x to (updateVisitCounter x))
    . filter canVisit
    $ successors from
 where
  updateVisitCounter index = Seq.adjust' (+ 1) index visitCounter
  canVisit v =
    T.toUpper (vertexName v)
      == vertexName v
      || fromJust (visitCounter Seq.!? v)
      <  1
      || (canVisitSmallCaveTwice && not (isSpecialVertex v))
  successors v = map snd . filter (\x -> fst x == v) $ edges graph
  vertexName v = S.elemAt v nodes
  canVisitSmallCaveTwice =
    not . any (\x -> T.toLower (fst x) == fst x && snd x > 1) $ zip
      (S.toList nodes)
      (toList visitCounter)
  isSpecialVertex v = S.elemAt v nodes `elem` [T.pack "start", T.pack "end"]

main = do
  (nodes, graph) <- parseFile
  print nodes
  let startVertex  = S.findIndex (T.pack "start") nodes
  let endVertex    = S.findIndex (T.pack "end") nodes
  let visitCounter = Seq.update startVertex 1 $ Seq.replicate (length nodes) 0
  print $ allPathFromTo nodes graph startVertex endVertex visitCounter
  -- Part 2
  print $ allPathFromTo2 nodes graph startVertex endVertex visitCounter

