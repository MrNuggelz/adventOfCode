import Advent
import Data.List.Split (splitWhen)
import Data.Set (fromList)
import Data.List (intersect)

run :: Show b => ([Group] -> b) -> IO ()
run = advent'' prepocess 6

test :: Show b => ([Group] -> b) -> IO ()
test = advent'' prepocess 61

type Group = [String]

prepocess :: String -> [Group]
prepocess x = splitWhen null $ lines x

part1 :: [Group] -> Int
part1 = sum . map (length . fromList . concat)

part2 :: [Group] -> Int
part2 = sum . map allYes

allYes :: Group -> Int
allYes = length . foldl intersect ['a'..'z']