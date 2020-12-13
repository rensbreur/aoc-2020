{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative (Applicative(liftA2))
import Control.Arrow ( Arrow(second) )
import Control.Monad.State ( replicateM_, gets, modify, evalState, execState, State )
import qualified Text.Parsec as P
import Text.Parsec.String (parseFromFile)

data Position = Position { x :: Int, y :: Int }
  deriving (Eq, Show)

data Action = MNorth | MEast | MSouth | MWest
            | RLeft | RRight
            | MForward

data Instruction = Instruction Action Int

class Navigation n where
  move :: Position -> n
  rotate :: Int -> n
  forward :: Int -> n
  (>-) :: n -> n -> n
  run :: n -> Position

navigate :: (Navigation n) => Instruction -> n
navigate (Instruction MNorth n) = move (Position   0  (-n))
navigate (Instruction MEast  n) = move (Position   n    0 )
navigate (Instruction MSouth n) = move (Position   0    n )
navigate (Instruction MWest  n) = move (Position (-n)   0 )
navigate (Instruction RRight degr) = rotate (degr `div` 90)
navigate (Instruction RLeft  degr) = rotate ((360 - degr) `div` 90)
navigate (Instruction MForward n) = forward n

-- Parsing

parseRoute :: P.Parsec String () [Instruction]
parseRoute = instruction `P.endBy` P.newline where
  instruction =  Instruction . action <$> P.letter <*> number
  action = \case
    {'N' -> MNorth; 'E' -> MEast; 'S' -> MSouth; 'W' -> MWest
    ;'L' -> RLeft; 'R' -> RRight; 'F' -> MForward             }
  number = read <$> P.many1 P.digit

-- Simple navigation

type Direction = Position 

newtype SimpleNavigation = SimpleNavigation { runSimple :: State Direction (Position -> Position) }

instance Navigation SimpleNavigation where
  
  move pos  = SimpleNavigation $ pure (+: pos)
  rotate n  = SimpleNavigation $ id <$ replicateM_ n (modify rotateClockwise)
  forward n = SimpleNavigation $ gets $ \d -> (+: (n *: d))

  (SimpleNavigation mv1) >- (SimpleNavigation mv2) = SimpleNavigation (liftA2 (flip (.)) mv1 mv2)

  run ms = evalState (runSimple ms) (Position 1 0) (Position 0 0)

-- Waypoint navigation

type Waypoint = Position

newtype WaypointNavigation = WaypointNavigation { runWaypoint :: State (Position, Waypoint) () }

instance Navigation WaypointNavigation where
  
  move pos  = WaypointNavigation . modify $ second (+: pos)
  rotate n  = WaypointNavigation $ replicateM_ n (modify $ second rotateClockwise)
  forward n = WaypointNavigation . modify $ \(position, waypoint) -> (position +: (n *: waypoint), waypoint)
  
  (WaypointNavigation mv1) >- (WaypointNavigation mv2) = WaypointNavigation (mv1 >> mv2)

  run ms = fst $ execState (runWaypoint ms) (Position 0 0, Position 10 (-1))

-- Main

main :: IO ()
main = parseFromFile parseRoute "ex12/input.txt" >>= \case
  Right route -> do
    putStrLn $ "Answer 1: " ++ show (run1 $ continue route)
    putStrLn $ "Answer 2: " ++ show (run2 $ continue route)
  Left err -> print err

run1 :: SimpleNavigation -> Position
run1 = run

run2 :: WaypointNavigation -> Position
run2 = run

continue :: (Navigation n) => [Instruction] -> n
continue = foldl1 (>-) . map navigate 

-- Utilities

(+:) :: Position -> Position -> Position
(Position x1 y1) +: (Position x2 y2) = Position (x1+x2) (y1+y2)

(-:) :: Position -> Position -> Position
(-:) v1 v2 = ((-1) *: v2) +: v1

(*:) :: Int -> Position -> Position
n *: (Position x y) = Position (n*x) (n*y)

manhattan :: Position -> Int
manhattan (Position x y) = x + y

rotateClockwise :: Position -> Position
rotateClockwise (Position x y) = Position (-y) x

