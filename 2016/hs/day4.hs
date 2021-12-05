import Data.Char (isLetter, ord, chr)
import Data.List (sortOn, isInfixOf)
import Data.MultiSet (fromList, toOccurList)
import Text.Regex.Posix ((=~))

data Room = Room { name :: String
                 , sectorId :: Int
                 , checksum :: String } deriving (Show)

parse :: String -> Room
parse input = 
    let [[_, roomName, roomSectorId, roomChecksum]] = input =~ "(.*)-([0-9]+)\\[(.*)\\]"
    in Room roomName (read roomSectorId) roomChecksum

isReal :: Room -> Bool
isReal room =
    let counts = fromList $ filter isLetter $ name room
        actualChecksum = map fst $ take 5 $ sortOn (\(char, count) -> (-count, char)) $ toOccurList counts
    in checksum room == actualChecksum

rotateChar :: Int -> Char -> Char
rotateChar _ '-' = ' '
rotateChar times c =
    let a = ord 'a'
        z = ord 'z'
        shifted = ord c + (times `mod` 26)
        coerced = if shifted > z then a + shifted - z - 1 else shifted
    in chr coerced

rotateName :: Room -> String
rotateName room = map (rotateChar $ sectorId room) $ name room

main :: IO ()
main = do
    input <- getContents
    let realRooms = filter isReal $ map parse $ lines input
    let encrypted = map (\room -> (rotateName room, room)) realRooms
    putStrLn $ "Sum of sector IDs: " ++ show (sum $ map sectorId realRooms)
    print $ filter (\(name, _) -> isInfixOf "north" name || isInfixOf "pole" name) encrypted