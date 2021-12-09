import Advent

advent1 :: ([Int] -> Int) -> IO ()
advent1 = advent 1

part1 = advent 1 part1'
part2 = advent 1 part2'

part1' :: [Int] -> Int
part1' (x : y : xs) = part1' (y : xs) + if x < y then 1 else 0
part1' _ = 0

part2' :: [Int] -> Int
part2' (a : b : c : d : xs) = part2' (b : c : d : xs) + if a < d then 1 else 0
part2' _ = 0