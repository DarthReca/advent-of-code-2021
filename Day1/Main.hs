module Day1.Main where
parseFile :: IO [Integer]
parseFile = do
    contents <- readFile "input.txt"
    let fileLines = map read $ lines contents :: [Integer]
    return fileLines

twoSlidingWindow :: [Integer] -> Int 
twoSlidingWindow depths = length . filter (\x -> snd x > fst x) . zip depths $ drop 1 depths


threeSlidingSum :: [Integer] -> [Integer]
threeSlidingSum depths = map (\(x,y,z) -> x + y + z) $ zip3 depths depths1 depths2
    where 
        depths1 = drop 1 depths
        depths2 = drop 2 depths

main :: IO ()
main = do 
    depths <- parseFile
    print $ twoSlidingWindow depths
    -- Part 2
    print . twoSlidingWindow $ threeSlidingSum depths 