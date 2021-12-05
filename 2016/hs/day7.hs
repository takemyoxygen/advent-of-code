import Data.Char (isLetter)
import Data.List (nub)

type IPv7 = String

isNonRepetitivePalindrome :: Int -> String -> Bool
isNonRepetitivePalindrome expectedLength input =
    let halfSize = (length input `quot` 2)
        firstHalf = take halfSize input
        uniqueHalf = nub firstHalf
    in all isLetter input &&
        length input == expectedLength &&
        length uniqueHalf == length firstHalf && 
        input == reverse input

iterateIp :: IPv7 -> a ->(IPv7 -> Bool -> a -> a) -> a
iterateIp ip initialState f = 
    let loop currentIp state bracketsDepth =
            case currentIp of 
            [] -> state
            ('[':rest) -> loop rest state (bracketsDepth + 1)
            (']':rest) -> loop rest state (bracketsDepth - 1)
            (_:rest) -> loop rest (f currentIp (bracketsDepth > 0) state) bracketsDepth
    in loop ip initialState 0

findRepetitivePalindromes :: IPv7 -> Int -> ([String], [String])
findRepetitivePalindromes ip targetLength = 
    iterateIp ip ([], []) (\currentIp insideHypernet (inside, outside) -> 
                            let candidate = take targetLength currentIp
                            in case (insideHypernet, isNonRepetitivePalindrome targetLength candidate) of
                                (True, True) -> (candidate:inside, outside)
                                (False, True) -> (inside, candidate:outside)
                                _ -> (inside, outside))

supportsTls :: IPv7 -> Bool
supportsTls ip =
    case findRepetitivePalindromes ip 4 of
    ([], (_:_)) -> True
    _ -> False

solve1 :: [IPv7] -> Int
solve1 ips = length $ filter supportsTls ips

supportsSsl :: IPv7 -> Bool
supportsSsl ip =
    let makeBab aba =(aba !! 1):(aba !! 0):[aba !! 1]
    in case findRepetitivePalindromes ip 3 of
        ([], _) -> False
        (inside, outside) -> any (\aba -> elem (makeBab aba) outside) inside

solve2 :: [IPv7] -> Int
solve2 ips = length $ filter supportsSsl ips

main :: IO ()
main = do
    ips <- lines <$> getContents
    print $ solve1 ips
    print $ solve2 ips