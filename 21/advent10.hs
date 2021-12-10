import Advent ( advent' )
import Data.Maybe (isJust, mapMaybe, isNothing)
import Data.Map ((!))
import Data.List (sort)

type Input = [String]

run :: Show a => (Input -> a) -> IO ()
run = advent' id 10

test :: Show a => (Input -> a) -> IO ()
test = advent' id 101

part1 :: Input  -> Int
part1 = sum . map (errorScore.snd.findIllegalCharacter []) where
    errorScore :: Maybe Char -> Int
    errorScore (Just ')') = 3
    errorScore (Just ']') = 57
    errorScore (Just '}') = 1197
    errorScore (Just '>') = 25137
    errorScore _ = 0

part2 :: [String] -> Int
part2 = getMiddle.sort.map (foldl (\x y -> x * 5 + y) 0.map score.finish "".fst) . filter (isNothing.snd) . map (findIllegalCharacter "") where
    finish :: String -> String -> String
    finish added [] = reverse added
    finish added (x:leftover) = finish (getClosing x:added) leftover

    getClosing :: Char -> Char
    getClosing x
        | x == '(' = ')'
        | x == '[' = ']'
        | x == '{' = '}'
        | x == '<' = '>'
        | otherwise = error $ "no closing found for: "  ++ [x]

    --openingMap :: [(Char,Char)]
    --openingMap = zip "([{<" ")]}>"

    score :: Char -> Int
    score ')' = 1
    score ']' = 2
    score '}' = 3
    score '>' = 4
    score _ = 0

    getMiddle :: [a] -> a
    getMiddle [] = error "no middle!"
    getMiddle [x] = x
    getMiddle l = getMiddle $ tail $ init l

findIllegalCharacter :: String -> String -> (String, Maybe Char)
findIllegalCharacter leftOver [] = (leftOver, Nothing)
findIllegalCharacter visited (x:xs)
    | isNotClosing x = findIllegalCharacter (x:visited) xs
    | x `isClosingFor` head visited = findIllegalCharacter (tail visited) xs
    | otherwise = (visited, Just x)

isClosingFor :: Char -> Char -> Bool
isClosingFor x y = getOpening x == y

closingMap :: [(Char,Char)]
closingMap = zip ")]}>" "([{<"


getOpening :: Char -> Char
getOpening x
    | x == ')' = '('
    | x == ']' = '['
    | x == '}' = '{'
    | x == '>' = '<'
    | otherwise = error $ "no opening found for: "  ++ [x]

isNotClosing :: Char -> Bool
isNotClosing = flip elem "([{<"