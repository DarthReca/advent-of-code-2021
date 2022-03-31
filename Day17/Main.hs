module Day17.Main where
import qualified Data.Bifunctor                as B
import           Data.Char                      ( isDigit )
import qualified Data.Text                     as T
import           Debug.Trace

parseFile = do
  contents <- readFile "input1.txt"
  let extractInts =
        listToTuple
          . map (\x -> read (T.unpack x) :: Int)
          . T.splitOn (T.pack "..")
          . T.dropWhile (\x -> not (isDigit x) && (x `notElem` ['-', '+']))
  let converted = map extractInts . T.splitOn (T.pack ",") $ T.pack contents
  return converted

doStep :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
doStep pos velocity =
  ( B.bimap (fst velocity +) (snd velocity +) pos
  , B.bimap (\x -> x + drag x) (\x -> x - 1) velocity
  )
 where
  drag x | x < 0     = 1
         | x > 0     = -1
         | otherwise = 0

listToTuple :: [a] -> (a, a)
listToTuple [a, b] = (a, b)
listToTuple a      = (head a, last a)

inArea :: [(Int, Int)] -> (Int, Int) -> Bool
inArea area pos =
  (inRange (head area) $ fst pos) && (inRange (last area) $ snd pos)
  where inRange (a, b) x = (x >= a) && (x <= b)

main = do
  input <- parseFile
  print input
  print $ until (\(pos, vel) -> trace (show (pos, vel)) . inArea input $ pos)
                (uncurry doStep)
                ((0, 0), (6, 9))
