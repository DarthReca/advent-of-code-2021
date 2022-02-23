module Day16.Main where

import qualified Data.Bifunctor
import           Data.Char                      ( digitToInt
                                                , intToDigit
                                                )
import           Data.List                      ( elemIndex )
import           Data.Maybe                     ( fromJust )
import qualified Data.Text                     as T
import           Debug.Trace
import           Numeric

conversionTable :: [String]
conversionTable =
  [ "0000"
  , "0001"
  , "0010"
  , "0011"
  , "0100"
  , "0101"
  , "0110"
  , "0111"
  , "1000"
  , "1001"
  , "1010"
  , "1011"
  , "1100"
  , "1101"
  , "1110"
  , "1111"
  ]

hexToBin :: Char -> String
hexToBin c = conversionTable !! (fst . head $ readHex [c])


binToDec :: String -> Int
binToDec b =
  sum . zipWith (\a b -> 2 ^ a * b) [0, 1 ..] $ reverse $ map digitToInt b

versionId :: String -> (Int, Int)
versionId bitString =
  (\(a, b) -> (binToDec $ '0' : a, binToDec $ '0' : b)) . splitAt 3 $ take
    6
    bitString

parseFile = do
  contents <- readFile "input1.txt"
  let converted = concatMap hexToBin contents
  return converted

parseLiteral :: String -> (Int, Int)
parseLiteral packet =
  (\x -> (length x, binToDec x))
    . concatMap (T.unpack . T.tail)
    -- . filter (\x -> T.length x == 5)
    . (\(a, b) -> a ++ [head b])
    . span (\x -> T.head x == '1')
    . T.chunksOf 5
    . T.pack
    $ drop 6 packet


parseOperator packet = parseSubpacket packet
 where
  lengthIdLenght | length packet < 7  = 0
                 | packet !! 6 == '0' = 15
                 | otherwise          = 11
  lengthValue         = binToDec . take lengthIdLenght $ drop 7 packet
  packetWithoutHeader = drop (7 + lengthIdLenght)
  subpackets p
    | lengthIdLenght == 15 = take lengthValue $ packetWithoutHeader p
    | lengthIdLenght == 11 = packetWithoutHeader p
    | otherwise            = ""
  lengthOfLiteral lit = 6 + fst (parseLiteral lit)
  parseSubpacket sub
    | null sub || length packet < 6 = trace "End" ([], "")
    | snd (versionId sub) == 4 = trace
      ("Literal : " ++ packet)
      ([fst (versionId sub)], drop (lengthOfLiteral sub) sub)
    | otherwise = trace
      ("Operator: " ++ packet)
      until
      (null . snd)
      (\x -> Data.Bifunctor.first (fst x ++) (parseOperator $ snd x))
      ([fst (versionId sub)], subpackets packet)

main = do
  number <- parseFile
  print $ fst (parseOperator number)
