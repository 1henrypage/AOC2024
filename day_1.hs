import Data.List (map)

parseFile :: FilePath -> IO [[Int]]
parseFile filePath = do
    contents <- readFile filePath
    let parsed = map (map read . words) (lines contents)
    return parsed

-- solveQ1 :: [[Int]] -> Int
-- solveQ1 reports = 

main :: IO ()
main = do
    parsedData <- parseFile "input.txt"
    print parsedData

