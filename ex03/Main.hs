module Main where

data Space = Space | Tree deriving Eq

type World = [[Space]]
type Journey = [Space]

readSpace :: Char -> Space
readSpace '.' = Space
readSpace '#' = Tree

readWorld :: String -> World
readWorld =  (map readSpace <$>) . lines

slope :: World -> Int -> Journey 
slope wrld dx = zipWith (\l xs -> cycle xs !! posInLine l) [0..] wrld
  where posInLine l = l * dx

skipEveryOther :: [a] -> [a]
skipEveryOther = map snd . filter (even . fst) . zip [0..]

countTrees :: Journey -> Int
countTrees = length . filter (== Tree)

main :: IO ()
main = do
  wrld <- readWorld <$> readFile "ex03/input.txt"
  let journey1 = slope wrld 3
  putStrLn $ "Answer 1: " ++ show (countTrees journey1)
  let journeys2 = map (slope wrld) [1,3,5,7] ++ [slope (skipEveryOther wrld) 1]
  putStrLn $ "Answer 2: " ++ show (product $ countTrees <$> journeys2)

