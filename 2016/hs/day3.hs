import System.IO (isEOF)
import Data.List.Split (chunksOf)
import Data.List (transpose)

type Lengths = [[Int]]

isTriangle :: [Int]-> Bool
isTriangle [a, b, c] =
    a + b > c && a + c > b && b + c > a

parseCoordinates :: [String] -> Lengths
parseCoordinates =
    map (map read . words)

rotate :: Lengths -> Lengths
rotate length = 
    let transposed = transpose length
    in transposed >>= chunksOf 3

solve :: (Lengths -> Lengths) ->[String] -> Int
solve transform =
    length . filter isTriangle . transform . parseCoordinates

main :: IO ()
main = do
    input <- getContents
    let count1 = solve id $ lines input
    putStrLn $ "#1: Number of triangles -  " ++ show count1
    let count2 = solve rotate $ lines input
    putStrLn $ "#2: Number of triangles -  " ++ show count2