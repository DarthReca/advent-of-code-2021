module Day10.Main where
import           Data.Either                    ( fromLeft
                                                , fromRight
                                                , isLeft
                                                )
import           Data.List
import           Data.Maybe

parseFile = do
  contents <- readFile "input.txt"
  let result = lines contents
  return result

openingChar :: Char -> Char
openingChar ')' = '('
openingChar ']' = '['
openingChar '}' = '{'
openingChar '>' = '<'
openingChar _   = ' '

score :: Maybe (Either Char Char) -> Int
score x | isNothing x            = 0
        | fromJust x == Left '(' = 3
        | fromJust x == Left '[' = 57
        | fromJust x == Left '{' = 1197
        | fromJust x == Left '<' = 25137
        | otherwise              = 0

pushOrPop :: [Either Char Char] -> Char -> [Either Char Char]
pushOrPop queue element
  | openingChar element == ' ' = queue ++ [Right element]
  | openingChar element == fromRight ' ' (last queue) = init queue
  | otherwise                  = queue ++ [Left $ openingChar element]

main = do
  contents <- parseFile
  let syntaxErrorScore =
        sum $ map (score . find isLeft . foldl pushOrPop []) contents
  print syntaxErrorScore
