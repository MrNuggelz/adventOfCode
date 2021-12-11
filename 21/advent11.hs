import Advent ( advent'' )
import Data.List (sort, (\\), findIndex)
import Data.Matrix (Matrix (ncols, nrows), fromLists, mapPos, setElem, safeGet, toList)
import Data.Char (digitToInt)
import Debug.Trace (traceShowId, trace, traceShow)
import Control.Arrow (second, Arrow (first))

type Input = Matrix Int

run :: Show a => (Input -> a) -> IO ()
run = advent'' preprocess 11

test :: Show a => (Input -> a) -> IO ()
test = advent'' preprocess 111

preprocess :: String -> Matrix Int
preprocess = fromLists . map (map digitToInt) . lines

part1 :: Input  -> Int
part1 = fst . applyN 100 step . (,) 0

part2 = findIndex (allFlashing.snd) . iterate step . (,) 0
--part2 = find (allFlashing.snd) . iterate step . (,) 0

allFlashing :: Matrix Int -> Bool
allFlashing = all (==0) . toList

step :: (Int, Matrix Int) -> (Int, Matrix Int)
step (nFlashed, m) = first ((nFlashed+).length) . foldr foo ([],m) $ cartProd [1..nrows m] [1..ncols m]

countFlashes :: Matrix Int -> Int
countFlashes = length . filter (==0) . toList

foo :: (Int, Int) -> ([(Int, Int)],Matrix Int) -> ([(Int, Int)],Matrix Int)
foo pos y = if pos `elem` fst y then y else incPos pos y

incPos :: (Int, Int) -> ([(Int, Int)],Matrix Int) -> ([(Int, Int)],Matrix Int)
incPos (x,y) (flashed,m) = case safeGet x y m of
    Just v -> if v == 9 then foldr foo ((x,y):flashed, setElem 0 (x,y) m) (surrounding (x,y)) else (flashed, setElem (v+1) (x,y) m)
    Nothing -> (flashed,m)

surrounding (x,y) = cartProd [x-1..x+1] [y-1..y+1] \\ [(x,y)]

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = applyN (n - 1) f (f x)

cartProd xs ys = [(x,y) | x <- xs, y <- ys]