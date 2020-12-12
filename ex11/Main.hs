module Main where

import Data.List ( find )
import Data.Maybe ( catMaybes, isJust, mapMaybe )
import qualified Data.Vector as V

type Ferry = Grid Space

type Grid a = V.Vector (V.Vector a)

data Space = Occupied | Empty | Floor deriving Eq

type Position = (Int, Int)

-- Parsing 

readFerry :: String -> Ferry
readFerry = V.fromList . (V.fromList . map f <$>) . lines
  where {f '.' = Floor; f 'L' = Empty; f '#' = Occupied}

-- Position helper functions

adjacent :: Position -> [Position]
adjacent (i, j) = [(i+n, j+m) | n <- [-1,0,1], m <- [-1,0,1], (n,m) /= (0,0)]

lookFrom :: Position -> [[Position]]
lookFrom (i, j) = flip map (adjacent (0, 0)) $ \(a, b) -> [(i + a*n, j + b*n) | n <- [1..]]

whileOn :: Grid a -> [Position] -> [a]
whileOn grid = catMaybes . takeWhile isJust . ((grid !?) <$>)

-- Boarding the ferry

seat1 :: Ferry -> Ferry 
seat1 ferry = flip imap ferry $ \i j space ->
  let adjacents = catMaybes $ (ferry !?) <$> adjacent (i,j) in 
  case space of
    Empty    | Occupied `notElem` adjacents   -> Occupied
    Occupied | count Occupied adjacents >= 4  -> Empty
    _                                         -> space

seat2 :: Ferry -> Ferry 
seat2 ferry = flip imap ferry $ \i j space ->
  let visibles = mapMaybe (find (/= Floor) . whileOn ferry) (lookFrom (i,j)) in
  case space of
    Empty    | Occupied `notElem` visibles  -> Occupied
    Occupied | count Occupied visibles >= 5 -> Empty
    _                                       -> space

board :: (Ferry -> Ferry) -> Ferry -> [Ferry]
board f = untilStable . iterate f

-- Main

main :: IO ()
main = do
  ferry <- readFerry <$> readFile "ex11/input.txt"
  putStrLn $ "Answer 1: " ++ show (answer seat1 ferry)
  putStrLn $ "Answer 2: " ++ show (answer seat2 ferry)
  where answer step = vCount Occupied . flatten . last . board step

-- Utilities

(!?) :: Grid a -> Position -> Maybe a 
as !? (i, j) = as V.!? i >>= (V.!? j)

imap :: (Int -> Int -> a -> b) -> Grid a -> Grid b
imap f = V.imap $ V.imap . f 

count :: (Eq a) => a -> [a] -> Int
count a = length . filter (== a)

vCount :: (Eq a) => a -> V.Vector a -> Int
vCount a = V.length . V.filter (== a)

untilStable :: (Eq a) => [a] -> [a]
untilStable xs = map fst . takeWhile (uncurry (/=)) $ zip xs (tail xs)

flatten :: Grid a -> V.Vector a
flatten = V.concatMap id

