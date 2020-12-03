module Main where

import Data.Foldable (find)
import Data.List (tails, sort)

pairs :: [a] -> [[a]]
pairs lst = [[x, y] | x:xs <- tails lst, y <- xs]

triples :: [a] -> [[a]]
triples lst = [x:pr | x:xs <- tails lst, pr <- pairs xs]

condition :: [Int] -> Bool
condition = (== 2020) . sum

answer1 :: [Int] -> Maybe Int
answer1 lst = product <$> find condition (pairs lst)

answer2 :: [Int] -> Maybe Int
answer2 lst = product <$> find condition (triples lst)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let lst = read <$> lines input
  let srtd = sort lst -- sorting improves speed
  putStrLn $ "Answer 1: " ++ show (answer1 srtd)
  putStrLn $ "Answer 2: " ++ show (answer2 srtd)

