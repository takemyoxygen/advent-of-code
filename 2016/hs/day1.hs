import Data.List.Split
import qualified Data.Set as Set

type Direction = Int -- 0 - North, 1 - East, 2 - South, 3 - West

data Point = Point Int Int deriving (Show, Ord, Eq)

data State = State Direction Point deriving (Show)

data Turn = L | R deriving (Show, Eq)

data Move = Move Turn Int

nextState :: State -> Move -> State
nextState (State direction (Point north east)) (Move turn blocks) =
    let nextDirection = ((if turn == L then -1 else 1) + direction) `mod` 4
        (northDelta, eastDelta) = case nextDirection of 0 -> (1, 0)
                                                        1 -> (0, 1)
                                                        2 -> (-1, 0)
                                                        3 -> (0, -1)
    in State nextDirection $ Point (north + northDelta * blocks) (east+ eastDelta * blocks)

waypoints :: [Move] -> [Point]
waypoints moves =
    map
        (\(State _ point) -> point) 
        (scanl nextState (State 0 $ Point 0 0) moves)

distanceTo :: Point -> Int
distanceTo (Point north east) = abs north + abs east

intermediatePoints :: [Point] -> [Point]
intermediatePoints (start:finish:rest) =
    let range a b = if a == b then [a] else [a, a + (signum $ b - a) .. b]
        between (Point x1 y1) (Point x2 y2) = [ Point x y | x <- range x1 x2,
                                                            y <- range y1 y2 ]
        after = intermediatePoints (finish:rest)
    in (between start finish) ++ (if null after then [] else tail after)
intermediatePoints _ = []

firstRepeat :: [Point] -> Maybe Point
firstRepeat points =
    let loop set (next:rest) = if Set.member next set then Just next else loop (Set.insert next set) rest
        loop _ [] = Nothing
    in loop Set.empty points

parse :: String -> [Move]
parse input = map parseMove $ splitOn ", " input where 
    parseMove (dir:length) = Move (if dir == 'L' then L else R) $ read length

main = do 
    putStrLn "Input:"
    input <- getLine
    let parsed = parse input
    let wps = waypoints parsed
    let finish = last wps
    let allPoints = intermediatePoints wps
    let firstVisitedTwice = firstRepeat $ allPoints
    putStrLn $ "Finish: " ++ (show finish)
    putStrLn $ "Distance: " ++ (show $ distanceTo finish)
    putStrLn $ "First point visited twice: " ++ (show firstVisitedTwice) ++ "; distance: " ++ (show $ fmap distanceTo firstVisitedTwice)