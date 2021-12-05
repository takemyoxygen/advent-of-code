import Data.List (transpose, sort, group, sortBy)
import Data.Function (on)
import Control.Arrow ((&&&))

mostAndLeastFrequent :: Ord a => [a] -> (a, a)
mostAndLeastFrequent words = 
    let counted = sortBy (compare `on` snd) $ map (head &&& length) $ group $ sort words
    in (fst $ last counted, fst $ head counted)

solve :: [String] -> (String, String)
solve = 
    let tmap f (x, y) = (f x, f y)
        tmap2 f (a, b) (x, y) = (f a x, f b y)
    in tmap reverse . foldl (flip $ tmap2 (:)) ("", "") . map mostAndLeastFrequent . transpose

main :: IO ()
main = do
    words <- lines <$> getContents
    print $ solve words