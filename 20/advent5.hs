import Advent
import Control.Arrow ((&&&))
import Data.List (sortBy, sort)

run :: Show b => ([String] -> b) -> IO ()
run = advent' id 5

data RowSearch = F | B deriving (Read, Show)
data ColumnSearch = L | R deriving (Read, Show)

--part1' :: [String] -> Int
part1 = maximum . map (uncurry (+)) . uncurry zip . (map ((*8).binaryFB) &&& map binaryLR)
part2 = findMissing . sort . map (uncurry (+)) . uncurry zip . (map ((*8).binaryFB) &&& map binaryLR)

findMissing :: [Int] -> Int
findMissing (x:y:xs)
    | x + 2 == y = x + 1
    | otherwise = findMissing (y:xs)
findMissing _ = error "Error!"

binaryFB :: String -> Int
binaryFB = head . foldl f [0..127] . map (read . (:[])) . take 7

f :: [Int] -> RowSearch -> [Int]
f [x] _ = [x]
f xs B = drop (halve xs) xs
f xs F = take (halve xs) xs

binaryLR :: String -> Int
binaryLR = head . foldl f' [0..7] . map (read . (:[])) . drop 7

f' :: [Int] -> ColumnSearch -> [Int]
f' [x] _ = [x]
f' xs R = drop (halve xs) xs
f' xs L = take (halve xs) xs

halve :: Foldable t => t a -> Int
halve xs = div (length xs) 2