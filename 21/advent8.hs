import Advent ( advent'' )
import Data.List.Split (splitWhen)
import Control.Arrow ((&&&))
import Control.Monad (ap)
import Data.Map (toList, Map, fromList, update, (!))
import Data.List (intersect, nub, (\\), sortOn, sort)
import Debug.Trace (traceShowId, trace, traceShow)

type Input = [([String], [String])]
--type Output = [([String], [String])]

data SevenSegment = Segments {
    a :: Char,
    b :: Char,
    c :: Char,
    d :: Char,
    e :: Char,
    f :: Char,
    g :: Char
}

run :: Show a => (Input -> a) -> IO ()
run = advent'' preprocess 8

test :: Show a => (Input -> a) -> IO ()
test = advent'' preprocess 81

--preprocess :: String -> Input
preprocess :: String -> [([String], [String])]
preprocess input = map f $ lines input where
    f :: String -> ([String], [String])
    f = (head &&& (!! 1)) . splitWhen (=="|") . words

--part1 :: Input -> Input
part1 :: Input -> Int
part1 = length . filter (flip elem [2,3,4,7].length) . concatMap snd

--part2 :: (Show a) => Input -> a abcdeg => cfgabepart1 :: Input -> Int cefadb , abcdef => cfgaed
part2 :: Input -> Int
part2 = sum . map (read.map (toSevenSegmentDigit.sort) . decode)

-- decode 

toSevenSegmentDigit :: String -> Char
toSevenSegmentDigit "abcefg" = '0'
toSevenSegmentDigit "cf" = '1'
toSevenSegmentDigit "acdeg" = '2'
toSevenSegmentDigit "acdfg" = '3'
toSevenSegmentDigit "bcdf" = '4'
toSevenSegmentDigit "abdfg" = '5'
toSevenSegmentDigit "abdefg" = '6'
toSevenSegmentDigit "acf" = '7'
toSevenSegmentDigit "abcdefg" = '8'
toSevenSegmentDigit "abcdfg" = '9'
toSevenSegmentDigit x = error $ "not known:" ++ x

decode :: ([String], [String]) -> [String]
decode (x,y) = flip applyMapping y $ foldl temp startMap $ sortOn length $ x ++ y

applyMapping :: Map Char [Char] -> [String] -> [String]
applyMapping mapping = map (concatMap (mapping !))

-- find Mapping

lengthToPossibilities x
 | x == 2 = ["cf"]
 | x == 3 = ["acf"]
 | x == 4 = ["bcdf"]
 | x == 5 = ["acdeg","acdfg","abdfg"]
 | x == 6 = ["abcefg","abdefg","abcdfg"]
 | x == 7 = ["abcdefg"]
 | otherwise  = [""]

findMapping :: [String] -> Map Char String
findMapping = foldl temp startMap

temp :: Map Char [Char] -> String -> Map Char [Char]
temp s x = try ((lengthToPossibilities.length) x) s x

startMap = fromList $ zip ['a'..'g'] $ repeat ['a'..'g']

try :: [String] -> Map Char String -> String -> Map Char String
try [] x y = error $ show (x,y)
try (target:xs) m source = let solution = intersectMap source target $ diffMap source target m in if validSolution solution then solution else try xs m source

validSolution :: Map Char String -> Bool
validSolution = not . hasDuplicates . filter (\ a -> length a < 2) . map snd . toList where
    hasDuplicates :: (Ord a) => [a] -> Bool
    hasDuplicates list = length list /= length (nub list)

intersectMap :: String -> String -> Map Char String ->  Map Char String
intersectMap source target state = foldr (update (\a -> Just $ intersect a target)) state source

diffMap :: String -> String -> Map Char String ->  Map Char String
diffMap source target state = foldr (update (\a -> Just $ a \\ target)) state (['a'..'g'] \\ source)
