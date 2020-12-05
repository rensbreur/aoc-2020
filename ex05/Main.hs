module Main where

import Data.List ( (\\) )
import Data.Maybe (listToMaybe)

-- replacing B and R by 1, and F and L by 0,
-- the boarding pass id is just the seat id in binary
seatId :: String -> Int
seatId = foldl (\b a -> 2*b + val a) 0
  where { val 'B' = 1; val 'R' = 1; val _   = 0 } 

findEmpty :: [Int] -> Maybe Int
findEmpty ids = listToMaybe $ [minimum ids..maximum ids] \\ ids

main :: IO ()
main = do
  input <- readFile "ex05/input.txt"
  let passes = seatId <$> lines input
  putStrLn $ "Answer 1: " ++ show (maximum passes)
  putStrLn $ "Answer 2: " ++ show (findEmpty passes)
