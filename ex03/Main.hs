module Main where

data Space = Space | Tree deriving (Eq)

type World = [[Space]]
type Path = [Space]

width :: World -> Int
width = length . head

readWorld :: String -> World
readWorld =  (map readSpace <$>) . lines
  where readSpace '.' = Space
        readSpace '#' = Tree

-- path in a world from the top left, moving up dx to the right for every move down
slope :: World -> Int -> Path 
slope wrld dx = zipWith spaceOnLine [0..] wrld
  where spaceOnLine n xs = xs !! ((n * dx) `mod` width wrld)

skipEveryOther :: [a] -> [a]
skipEveryOther = map snd . filter (even . fst) . zip [0..]

numberOfTrees :: Path -> Int
numberOfTrees = length . filter (== Tree)

main :: IO ()
main = do
  wrld <- readWorld <$> readFile "input.txt"
  let path1 = slope wrld 3
  putStrLn $ "Answer 1: " ++ show (numberOfTrees path1)
  let paths2 = map (slope wrld) [1,3,5,7] ++ [slope (skipEveryOther wrld) 1]
  putStrLn $ "Answer 2: " ++ show (product $ numberOfTrees <$> paths2)

