{-# LANGUAGE TupleSections #-}
import Advent
import Control.Monad (liftM2)
import Data.Foldable (find, Foldable (fold))
import Data.List (group, isInfixOf, sort, transpose, (\\), intersperse, intercalate)
import Data.List.Split ( splitOn )
import GHC.IO (unsafePerformIO)
import Data.Map (Map, empty, alter, toList)
import Data.Maybe (fromMaybe)

part1 = advent' preprocess 5 part1'
part2 = advent' preprocess 5 part2'

type Vent = ((Int,Int),(Int,Int))
type VisitedPoints = Map (Int,Int) Int

preprocess :: String -> Vent
preprocess = read . surroundWithBrackets . intercalate "," . map surroundWithBrackets . splitOn " -> " where
  surroundWithBrackets = ("(" ++) . (++ ")")

part1' :: [Vent] -> Int
part1' = part2' . filterVents where
  filterVents :: [Vent] -> [Vent]
  filterVents = filter $ liftM2 (||) (compareTuples fst) $ compareTuples snd where
    compareTuples :: Eq a => (b -> a) -> (b, b) -> Bool
    compareTuples f = liftM2 (==) (f.fst) (f.snd)

part2' :: [Vent] -> Int
part2' = length . filter (>1) . map snd . toList . visitPoints

visitPoints :: [Vent] -> VisitedPoints
visitPoints = flipFold walkVent empty

walkVent :: Vent -> VisitedPoints -> VisitedPoints
walkVent = flip (flipFold (alter maybeIncrement)) . ventToList

ventToList :: Vent -> [(Int,Int)]
ventToList ((x1, x2), (y1, y2))
  | x1 == y1 = map (x1,) $ between x2 y2
  | x2 == y2 = map (,x2) $ between x1 y1
  | otherwise = zip (between x1 y1) (between x2 y2)

between :: Int -> Int -> [Int]
between x y = if x < y then [x..y] else [x,x-1..y]

flipFold :: Foldable t => (a -> b -> b) -> b -> t a -> b
flipFold = foldl . flip

maybeIncrement :: Maybe Int -> Maybe Int
maybeIncrement = Just . (+ 1) . fromMaybe 0

