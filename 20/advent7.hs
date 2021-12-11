import Advent
import Data.List.Split (splitWhen, splitOn)
import Data.Set (fromList)
import Data.List (intersect, find, nub)
import Control.Arrow (Arrow(second, (&&&)))
import Data.Char (digitToInt)
import Debug.Trace (traceShow, traceShowId)
import Data.Maybe (fromJust)

type Input = (String, [(Int, String)])
type Inputs = [Input]

run :: Show b => (Inputs -> b) -> IO ()
run = advent' preprocess 7

test :: Show b => (Inputs -> b) -> IO ()
test = advent' preprocess 71

preprocess :: String -> Input
preprocess x = second (map formatBagRule . filter (/="no other bags.") . splitOn ", ") $ listToTuple $ splitOn " bags contain " x

formatBagRule :: String -> (Int, String)
formatBagRule = digitToInt.head &&& unwords.tail.init . words

listToTuple :: [a] -> (a,a)
listToTuple [x, y] = (x,y)
listToTuple _ = error "wrong input"

----

part1 :: Inputs -> Int
part1 rules = length . nub . concatMap (accumulateContaining rules) $ containingShinyGold rules where
    accumulateContaining :: Inputs -> String -> [String]
    accumulateContaining rules color = let newColors = containung rules color in color : newColors ++ concatMap (accumulateContaining rules) newColors

    containung :: (Foldable t, Eq a1) => [(b, t (a2, a1))] -> a1 -> [b]
    containung rules color = map fst (filter (any ((==color).snd).snd) rules)

    containingShinyGold :: Inputs -> [String]
    containingShinyGold = flip containung "shiny gold"

---

part2 :: Inputs -> Int
part2 rules = sum $ map containing $ doesContain "shiny gold" rules where
    containing :: (Int,String) -> Int
    containing (n,color) = let newColors = doesContain color rules in n * (1 + sum (map containing newColors))

    doesContain color = snd . fromJust . find ((==color).fst)