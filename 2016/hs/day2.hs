import System.IO (isEOF)
import Prelude hiding (length)
import Data.Vector ((!), fromList, Vector, length)

type Numpad = Vector (Vector Char)
type Point = (Int, Int)

numpad1 :: Numpad
numpad1 = fromList $ map fromList [
    "123", 
    "456", 
    "789"] 

numpad2 :: Numpad
numpad2 = fromList $ map fromList [
    "  1  ", 
    " 234 ", 
    "56789", 
    " ABC ", 
    "  D  "]

advance :: Numpad -> Point -> String -> Point
advance numpad start input = 
    let limit = length numpad
        mv (x, y) 'U' = (x, y-1)
        mv (x, y) 'D' = (x, y+1)
        mv (x, y) 'L' = (x-1, y)
        mv (x, y) 'R' = (x+1, y)
        isOk (x, y) = x >= 0 && x < limit && y >= 0 && y < limit && ((numpad ! y ! x) /= ' ')
        move point direction =
            let next = mv point direction
            in if isOk next then next else point
    in foldl move start input
    
solve :: Numpad -> Point -> [String] -> String
solve numpad start input =
    let elem (x, y) = numpad ! y ! x
    in (map elem . tail . (scanl (advance numpad) start)) input

readInput :: [String] -> IO [String]
readInput input = do
    done <- isEOF
    if done
        then return $ reverse input
        else do
            line <- getLine
            readInput (line:input)

main = do   
    lines <- readInput []
    let code1 = solve numpad1 (1, 1) lines
    putStrLn code1
    let code2 = solve numpad2 (0, 2) lines
    putStrLn code2