import Advent
import Control.Arrow (first, second, (***))
import Control.Monad (ap, liftM2)
import Data.Char ()
import Data.List (group, sort, transpose)
import Data.Tuple ()

part1 = advent' id 3 part1'

part2 = advent' id 3 part2'

part1' :: [[Char]] -> Int
part1' = liftM2 (*) gamma epsilon
  where
    calcValue :: ([Char] -> Char) -> [[Char]] -> Int
    calcValue f = bintodec . read . map f . transpose

    gamma = calcValue mostCommon
    epsilon = calcValue leastCommon

part2' :: [[Char]] -> Int
part2' = liftM2 (*) oxygen co2
  where
    calcValue :: ([Char] -> Char) -> [[Char]] -> Int
    calcValue f = bintodec . read . filterToLast f . map dupe

    filterToLast :: ([Char] -> Char) -> [([Char], [Char])] -> [Char]
    filterToLast f [] = "No life support rating found"
    filterToLast f [x] = snd x
    filterToLast f xs =
      let bit = (head . map f . transpose . map fst) xs
       in filterToLast f $ map (first tail) $ filter ((== bit) . head . fst) xs

    oxygen = calcValue mostCommon
    co2 = calcValue leastCommon

bintodec :: Integral i => i -> i
bintodec 0 = 0
bintodec i = 2 * bintodec (div i 10) + mod i 10

mostCommon :: Ord a => [a] -> a
mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort

leastCommon :: Ord a => [a] -> a
leastCommon = snd . minimum . map (\xs -> (length xs, head xs)) . group . sort

dupe :: [b] -> ([b], [b])
dupe x = (x, x)