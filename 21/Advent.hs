module Advent where

advent :: (Read a, Show b) => Int -> ([a] -> b) -> IO ()
advent = advent' read

advent' :: (Show b) => (String -> a) -> Int -> ([a] -> b) -> IO ()
advent' mapping  = advent'' (map mapping . lines)

advent'' :: (Show b) => (String -> a) -> Int -> (a -> b) -> IO ()
advent'' mapping n f  = readFile ("advent" ++ show n ++ ".txt") >>= (print . f . mapping)

test :: (Read a, Show b) => Int -> ([a] -> b) -> IO ()
test x = advent' read (x*10+1)