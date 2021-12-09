import Advent ( advent'', advent' )
import Data.List.Split (splitOn)
import Control.Monad (liftM2)
import Control.Monad.Cont (ap)
import Data.Matrix (fromLists, Matrix (ncols, nrows), (!), safeGet)
import Data.Char (digitToInt)
import Data.Maybe (mapMaybe, isJust)
import Data.List (sortOn, sort, (\\), nub)

run :: Show a => ([[Int]] -> a) -> IO ()
run = advent' (map digitToInt) 9

test :: Show a => ([[Int]] -> a) -> IO ()
test = advent' (map digitToInt) 91

part1 :: [[Int]]  -> Int
part1 = sum . map (+1) . findLowPointHeights . fromLists where
    findLowPointHeights :: Matrix Int -> [Int]
    findLowPointHeights m = map (m !) (findLowPoints m)

input = map (map digitToInt) . lines <$> readFile "advent91.txt"

part2 :: [[Int]] -> Int
part2 = calculateBasin . fromLists

calculateBasin :: Matrix Int -> Int
calculateBasin m = product $ take 3 $ reverse $ sort $ map (length.findBasin m) $ findLowPoints m

findBasin :: Matrix Int -> (Int, Int) -> [(Int, Int)]
findBasin m x = walkBasin m [x] []

walkBasin :: Matrix Int -> [(Int, Int)] -> [(Int, Int)] -> [(Int,Int)]
walkBasin _ [] visited = nub visited
walkBasin m (found:xs) visited = walkBasin m ((getSurroundingCords m found ++ xs) \\ visited) (found:visited)

getSurroundingCords :: Matrix Int -> (Int, Int) -> [(Int,Int)]
getSurroundingCords m = filter (isJustAndNot9 . safeGet' m) . surrounding where
    isJustAndNot9 :: Maybe Int -> Bool
    isJustAndNot9 Nothing = False
    isJustAndNot9 (Just a) = a /= 9

surrounding :: (Int,Int) -> [(Int,Int)]
surrounding (x,y) = [(x-1,y),(x,y-1),(x+1,y),(x,y+1)]

safeGet' :: Matrix a -> (Int, Int) -> Maybe a
safeGet' = flip (uncurry safeGet)

findLowPoints :: Matrix Int -> [(Int,Int)]
findLowPoints m = filter (isLowest m) $ cartProd [1..nrows m] [1..ncols m] where
    cartProd xs ys = [(x,y) | x <- xs, y <- ys]

isLowest :: Matrix Int -> (Int,Int) -> Bool
isLowest m x = all (>(m ! x)) $ getSurrounding x m

getSurrounding :: (Int, Int) -> Matrix Int -> [Int]
getSurrounding (x,y) m = mapMaybe (safeGet' m) [(x-1,y),(x,y-1),(x+1,y),(x,y+1)]
