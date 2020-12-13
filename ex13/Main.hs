{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List (minimumBy, elemIndex, sortOn, foldl1', iterate')
import Data.Maybe (catMaybes, mapMaybe, fromJust)
import Data.Ord (comparing, Down(Down))
import Text.Parsec 
import Text.Parsec.String (parseFromFile)

type Time = Int
type BusID = Int

parseInput :: Parsec String () (Time, [Maybe BusID])
parseInput = (,) <$> time <* newline <*> schedule where
  time = read <$> many1 digit
  schedule = (Just <$> time <|> Nothing <$ char 'x') `sepBy` char ','

main :: IO ()
main = parseFromFile parseInput "ex13/input.txt" >>= \case
  Right (arrival, schedule) -> do
    let (bestBus, waiting) = findBest arrival (catMaybes schedule)
    putStrLn $ "Answer 1: " ++ show (bestBus * waiting)
    let withIndex = sortOn (Down . snd) $ mapMaybe f $ zip [0..] schedule
    putStrLn $ "Answer 2: " ++ show (uncurry waitingTime $ foldl1' combine withIndex)
  Left err -> print err
  where f (i, Just j ) = Just (i,j)
        f (_, Nothing) = Nothing

waitingTime :: Time -> BusID -> Time
waitingTime arrival busId = (- arrival) `mod` busId

findBest :: Time -> [BusID] -> (BusID, Time)
findBest arrival busId = minimumBy (comparing snd) (zip busId (waitingTime arrival <$> busId))

{- Efficiently find the first time t that both
    (t+i1) `mod` busId1
   and 
    (t+i2) `mod` busId2
   are 0.
 -}
firstTime :: (Int, BusID) -> (Int, BusID) -> Time
firstTime (i1, busId1) (i2, busId2) =
  let waitingTimes2 = iterate' ((`mod` busId2) . (+ busId1)) ((busId1 - i1 + i2) `mod` busId2)
      matchIndex    = fromJust $ elemIndex 0 waitingTimes2
  in  busId1 - i1 + matchIndex * busId1

combine :: (Int, BusID) -> (Int, BusID) -> (Int, BusID)
combine (i1, busId1) (i2, busId2) =
  let newTime  = firstTime (i1, busId1) (i2, busId2)
      newBusID = busId1 * busId2
  in  (newBusID - newTime, newBusID)

