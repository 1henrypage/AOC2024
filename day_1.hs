import System.IO
import Data.List
import GHC.Read (readField)

parseFile :: FilePath -> IO [String]
parseFile filePath = do
    contents <- readFile filePath 
    return (lines contents)

parseLineQ1 :: String -> (Int, Int)
parseLineQ1 line = 
    let [x, y] = map read (words line)
    in (x,y)

parseQuestionLine :: IO [String] -> IO ([Int], [Int])
parseQuestionLine lines = do 
    linesNonM <- lines
    let parsedData = map parseLineQ1 linesNonM
    let (col1, col2) = unzip parsedData
    return (col1, col2)

countElement :: Eq a => a -> [a] -> Int
countElement x = length . filter (== x)

solveQ1 :: [Int] -> [Int] -> Int
solveQ1 l1 l2 = sum $ zipWith (\a b -> abs(a-b)) (sort l1) (sort l2)

solveQ2 :: [Int] -> [Int] -> Int
solveQ2 l1 l2 = sum $ map (\a -> a * countElement a l2) l1


main :: IO ()
main = do
    (col1, col2) <- parseQuestionLine $ parseFile "input.txt"
    print $ solveQ1 col1 col2
    print $ solveQ2 col1 col2
