module Advent where

advent :: (Read a, Show b) => Int -> ([a] -> b) -> IO ()
advent = advent' read

advent' :: (Read a, Show b) => (String -> a) -> Int -> ([a] -> b) -> IO ()
advent' mapping  = advent'' (map mapping . lines)

advent'' :: (Read a, Show b) => (String -> a) -> Int -> (a -> b) -> IO ()
advent'' mapping n f  = readFile ("advent" ++ show n) >>= (print . f . mapping)