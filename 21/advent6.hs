import Advent ( advent'' )
import Data.List.Split (splitOn)
import Data.IntMap (fromList, toList, empty, IntMap, alter, foldrWithKey)
import Data.List (sort, group)
import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe)

part1 = advent'' preprocess 6 (processing 80)
part2 = advent'' preprocess 6 (processing 256)

type Population = IntMap Int

preprocess :: String -> [Int]
preprocess = map read.splitOn ","

processing :: Int -> [Int] -> Int
processing n = sum . map snd . toList . applyN n (foldrWithKey simulateDay empty) . frequency

simulateDay :: Int -> Int -> IntMap Int -> IntMap Int
simulateDay 0 v = alter (maybeAdd v) 8 . alter (maybeAdd v) 6
simulateDay x v = alter (maybeAdd v) (x-1)

maybeAdd :: Int -> Maybe Int -> Maybe Int
maybeAdd x = Just . (+ x) . fromMaybe 0

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = applyN (n - 1) f (f x)

frequency :: [Int] -> IntMap Int
frequency = fromList . map (head &&& length) . group . sort