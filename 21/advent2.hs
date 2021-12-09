import Advent (advent')
import Control.Arrow (first, second, (***))
import Data.Char

capitalise :: String -> String
capitalise (x : xs) = toUpper x : xs
capitalise [] = []

advent2 :: ([Command] -> Int) -> IO ()
advent2 = advent' (read . capitalise) 2

part1 :: [Command] -> Int
part1 = uncurry (*) . foldl (flip runCommand) (0, 0)
  where
    runCommand :: Command -> (Int, Int) -> (Int, Int)
    runCommand (Forward n) = second (+ n)
    runCommand (Down n) = first (+ n)
    runCommand (Up n) = first (flip (-) n)

part2 :: [Command] -> Int
part2 = uncurry (*) . fst . foldl runCommand ((0, 0), 0)
  where
    runCommand :: ((Int, Int), Int) -> Command -> ((Int, Int), Int)
    runCommand ((h, d), a) (Forward n) = ((h + n, d + a * n), a)
    runCommand x (Down n) = second (+n) x
    runCommand x (Up n) = second (flip (-) n) x

data Command = Forward Int | Down Int | Up Int deriving (Read, Show)