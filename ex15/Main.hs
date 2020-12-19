{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.IntMap.Strict as IM
import Data.List

setup :: [Int] -> (Int, Int, IM.IntMap Int)
setup (a:as) = foldl (\(i, x, m) x' -> (i + 1, x', IM.insert x i m)) (0, a, IM.empty) as

turn :: (Int, Int, IM.IntMap Int) -> (Int, Int, IM.IntMap Int)
turn (i, !x, m) = 
  let x' = case m IM.!? x of
             Just mx -> i - mx
             Nothing -> 0
  in (i+1, x', IM.insert x i m)

turnN :: (Int, Int, IM.IntMap Int) -> Int -> Int 
turnN (i,x,m) n | i == n    = x
                | otherwise = turnN (turn (i,x,m)) n

game :: [Int] -> Int -> Int
game starts = turnN $ setup starts

main :: IO ()
main = do
  let input = [2,15,0,9,1,20]
  putStrLn $ "Answer 1" ++ show (game input (2020-1))
  putStrLn $ "Answer 2" ++ show (game input (30000000-1))

