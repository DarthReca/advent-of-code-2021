module Day2.Main where

import Data.Bifunctor

parseFile :: IO [[String]]
parseFile = do
    contents <- readFile "input.txt"
    let fileWords = map words $ lines contents
    return fileWords

directionToTuple :: [String] -> (Int, Int)
directionToTuple tup
    | head tup == "down" = (toInt, 0)
    | head tup == "up" = (negate toInt, 0)
    | otherwise = (0,toInt)
    where toInt = read $ tup !! 1 :: Int

directionToTupleWithAim :: [String] -> (Int, Int, Int)
directionToTupleWithAim tup
    | head tup == "down" = (0,0,toInt)
    | head tup == "up" = (0,0,negate toInt)
    | otherwise = (0, toInt, 0)
    where toInt = read $ tup !! 1 :: Int

main :: IO ()
main = do
    fileWords <- parseFile
    let res = foldl1 (\a b -> bimap (fst a +) (snd a +) b) $ map directionToTuple fileWords
    print $ uncurry (*) res
    -- Part 2
    let res2 = foldl1 (\(x,y,z) (a,b,c) -> (x + z*b, y+b, z+c)) $ map directionToTupleWithAim fileWords
    print $ (\(a,b,c) -> a * b) res2