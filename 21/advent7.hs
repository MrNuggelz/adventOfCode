import Advent ( advent'' )
import Data.List.Split (splitOn)
import Control.Monad (liftM2)
import Control.Monad.Cont (ap)

advent7 :: Show a => ([Int] -> a) -> IO ()
advent7 = advent'' (map read.splitOn ",") 7

test :: Show a => ([Int] -> a) -> IO ()
test = advent'' (map read.splitOn ",") 71

f :: (Int -> Int -> Int) -> [Int] -> Int
f distance = minimum . ap (map . flip (costOf distance)) possibleRows

part1 :: [Int] -> Int
part1 = f ((abs .) . subtract)

part2 :: [Int] -> Int
part2 = f distance where
    distance :: Int -> Int -> Int
    distance row x = let n = abs (x - row) in div (n * (n + 1)) 2

costOf :: (Int -> Int -> Int) -> Int -> [Int] -> Int
costOf distance row = sum . map (distance row)

possibleRows :: [Int] -> [Int]
possibleRows = liftM2 enumFromTo minimum maximum
