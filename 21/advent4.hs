import Advent
import Control.Arrow (first, second, (&&&), (***))
import Control.Monad (ap, liftM2)
import Data.Foldable (find)
import Data.List (group, isInfixOf, sort, transpose, (\\))
import Data.List.Split ( splitOn, splitWhen )
import Data.Tuple ()
import GHC.IO (unsafePerformIO)

part1 = advent'' preprocess 4 part1'

part2 = advent'' preprocess 4 part2'

type RandomNumbers = [Int]

type Board = [[Int]]

preprocess :: String -> (RandomNumbers, [Board])
preprocess = (map read . splitOn "," . head . lines) &&& (map (map (map read . words)) . splitWhen (== "") . drop 2 . lines)

part1' :: (RandomNumbers, [Board]) -> Int
part1' (random, boards) = playBingo 1 boards
  where
    playBingo :: Int -> [Board] -> Int
    playBingo n boards = case calcWinner (take n random) boards of
      Nothing -> playBingo (n + 1) boards
      Just a -> a

    calcWinner :: RandomNumbers -> [Board] -> Maybe Int
    calcWinner numbers boards = do
      winner <- find (winningCondition numbers) boards
      return (sum (concat winner \\ numbers) * last numbers)

part2' :: (RandomNumbers, [Board]) -> Int
part2' (numbers, boards) = findLastBoard 1 boards
  where
    findLastBoard :: Int -> [Board] -> Int
    findLastBoard _ [] = error "No last Board found!"
    findLastBoard n [x] = playUntilFinished x n
    findLastBoard n xs = findLastBoard (n + 1) $ filter (not . winningCondition (take n numbers)) xs

    playUntilFinished :: Board -> Int -> Int
    playUntilFinished b n =
      if winningCondition (take n numbers) b
        then sum (concat b \\ take n  numbers) * last (take n  numbers)
        else playUntilFinished b (n + 1)

winningCondition :: RandomNumbers -> Board -> Bool
winningCondition numbers board = winning numbers board || winning numbers (transpose board)
  where
    winning :: RandomNumbers -> Board -> Bool
    winning numbers = any (`containsAll` numbers)

containsAll :: [Int] -> [Int] -> Bool
containsAll = (null .) . (\\)