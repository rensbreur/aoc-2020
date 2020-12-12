module Main where

import Data.List ( find, inits, tails, transpose )
import Data.Maybe ( mapMaybe )

readNumbers :: String -> [Int]
readNumbers = fmap read . lines

withLast :: Int -> [Int] -> [(Int, [Int])]
withLast g xs = let prev = transpose $ map (`drop` xs) [0..(g-1)]
                in  zip (drop g xs) prev

validate :: (Int, [Int]) -> Bool
validate (n, lst) = let sums = [x + y | x:xs <- tails lst, y <- xs]
                    in  n `elem` sums

lookForSum :: Int -> [Int] -> Maybe [Int]
lookForSum a bs = do
  let comp = compare a <$> scanl (+) 0 bs
  (ord, xs) <- find ((/= GT) . fst) $ zip comp (inits bs)
  if ord == EQ then Just xs else Nothing

weakness :: [Int] -> Int
weakness xs = maximum xs + minimum xs

main :: IO ()
main = do
  input <- readNumbers <$> readFile "ex09/input.txt"
  let (answer1, _) = head . filter (not . validate) . withLast 25 $ input
  putStrLn $ "Answer 1: " ++ show answer1
  let answer2 = weakness . head . mapMaybe (lookForSum answer1) $ tails input
  putStrLn $ "Answer 2: " ++ show answer2

