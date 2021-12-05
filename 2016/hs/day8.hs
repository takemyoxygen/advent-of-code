import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Array

type Grid = Array (Int, Int) Bool

screenWidth = 50
screenHeight = 6

data Instruction
    = Rect Int Int
    | RotateRow Int Int
    | RotateColumn Int Int
    deriving (Show)

parseRect :: String -> Maybe Instruction
parseRect input =
    (\[x, y] -> Rect x y)
    <$> (map read . splitOn "x")
    <$> (stripPrefix "rect " input)

parseRotate :: String -> Maybe Instruction
parseRotate input =
    case stripPrefix "rotate " input of
    Just left ->
        case splitOn " " left of
        ["column", ('x':'=':coord), "by", step] -> Just $ RotateColumn (read coord) (read step)
        ["row", ('y':'=':coord), "by", step] -> Just $ RotateRow (read coord) (read step)
        _ -> Nothing
    _ -> Nothing

parse :: String -> Instruction
parse line =
    let parsers = [parseRect, parseRotate]
    in head $ mapMaybe (\parser -> parser line) parsers

emptyGrid :: Grid
emptyGrid = array ((0, 0), (screenWidth - 1, screenHeight - 1)) [((x, y), False) | x <- [0..screenWidth - 1], y <- [0..screenHeight - 1]]

applyInstruction :: Grid -> Instruction -> Grid
applyInstruction grid (Rect width height) =
    grid // [((x, y), True) | x <- [0..width - 1], y <- [0..height - 1]]
applyInstruction grid (RotateRow y step) =
    grid // [((i, y), (grid ! ((screenWidth + i - step) `rem` screenWidth, y))) | i <- [0..screenWidth - 1]]
applyInstruction grid (RotateColumn x step) =
    grid // [((x, i), (grid ! (x, (screenHeight + i - step) `rem` screenHeight))) | i <- [0..screenHeight - 1]]

applyInstructions :: Grid -> [Instruction] -> Grid
applyInstructions grid instructions =
    foldl applyInstruction grid instructions

printGrid :: Grid -> IO [()]
printGrid grid = sequence $ map (putStrLn . textRepresentation) $ toSimpleArray grid

toSimpleArray :: Grid -> [[Bool]]
toSimpleArray grid = [[grid ! (x, y) | x<-[lowx..highx]] |  y<-[lowy..highy]]
    where ((lowx, lowy), (highx, highy)) =  bounds grid

textRepresentation :: [Bool] -> String
textRepresentation row = foldl (\acc y -> acc ++ (if y then "#" else ".")) "" row

main :: IO ()
main = do
    input <- lines <$> getContents
    let instructions = map parse input
    let grid = applyInstructions emptyGrid instructions
    printGrid grid
    let switchedOn = (length . filter id . elems) $ grid
    print switchedOn