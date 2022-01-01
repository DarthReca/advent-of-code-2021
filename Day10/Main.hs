module Day10.Main where
import           Data.Either                    ( fromLeft
                                                , fromRight
                                                , isLeft
                                                , isRight
                                                )
import           Data.List                      ( find
                                                , sort
                                                )
import           Data.Map                       ( alterF )
import           Data.Maybe                     ( fromJust
                                                , isNothing
                                                )

parseFile :: IO [String]
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

errorScore :: Maybe (Either Char Char) -> Int
errorScore x | isNothing x            = 0
             | fromJust x == Left '(' = 3
             | fromJust x == Left '[' = 57
             | fromJust x == Left '{' = 1197
             | fromJust x == Left '<' = 25137
             | otherwise              = 0

autocompleteScore :: Either Char Char -> Int
autocompleteScore (Right '(') = 1
autocompleteScore (Right '[') = 2
autocompleteScore (Right '{') = 3
autocompleteScore (Right '<') = 4
autocompleteScore _           = 0


-- |This is like a queue with automated push or pop.
-- Return the queue with with a pushed Right in case of opening paranthesis or a popped Right in case of closing parenthesis or a pushed Left in case of error
pushOrPop :: [Either Char Char] -> Char -> [Either Char Char]
pushOrPop queue element
  | openingChar element == ' ' = queue ++ [Right element]
  | openingChar element == fromRight ' ' (last queue) = init queue
  | otherwise                  = queue ++ [Left $ openingChar element]

main = do
  contents <- parseFile
  let rightAndLefts    = map (foldl pushOrPop []) contents
  let syntaxErrorScore = sum $ map (errorScore . find isLeft) rightAndLefts
  print syntaxErrorScore
  -- Part 2
  let incompletes = filter (all isRight) rightAndLefts
  let incompletesScores = sort
        $ map (foldr (\x acc -> acc * 5 + autocompleteScore x) 0) incompletes
  print $ incompletesScores !! (length incompletesScores `quot` 2)
