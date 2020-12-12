{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List ( group, sort )

readNumbers :: String -> [Int]
readNumbers = fmap read . lines

diffs :: [Int] -> [Int]
diffs xs = zipWith (-) (tail xs) xs

joltageJumps :: [Int] -> [Int]
joltageJumps js = diffs (0 : js) ++ [3]

{- If there are only jumps of 1 (< four in a row) and 3, adapters can
   be removed only in such a way, that only neighbouring jumps of 1
   are combined.
   * 3 * 3 *                  cannot be combined
   * 3 * 1 * 3 *              cannot be combined
   * 3 * 1 * 1 * 3 *          can be combined once (1x)
   * 3 * 1 * 1 * 1 * 3 *      can be combined once (2x) or twice (1x)
   * 3 * 1 * 1 * 1 * 1 * 3 *  can be combined once (3x) or twice (3x)
 -}
countArrangements :: [Int] -> Int
countArrangements deltas = product (f <$> countContOnes) where
  countContOnes = length <$> (filter (all (== 1)) . group $ deltas)
  f = \case {0 -> 1; 1 -> 1; 2 -> 2; 3 -> 4; 4 -> 7}

count :: (Eq a) => a -> [a] -> Int
count a = length . filter (== a)

main :: IO ()
main = do
  input <- sort . readNumbers <$> readFile "ex10/input.txt"
  let deltas = joltageJumps input
  putStrLn $ "Answer 1: " ++ show (count 3 deltas * count 1 deltas)
  putStrLn $ "Answer 2: " ++ show (countArrangements deltas)

