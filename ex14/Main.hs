{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Bits ( Bits((.&.)) )
import qualified Data.Map as M
import qualified Text.Parsec as P
import Text.Parsec.String (parseFromFile)

data Instruction = SetBitmask String | SetMemoryValue Int Int
  deriving Show

data Bitmask = Bitmask { bmMask :: Int, bmValue :: Int }
  deriving Show

mask :: Bitmask -> Int -> Int
mask bm n = bmMask bm .&. n + bmValue bm

mask2 :: [Bitmask] -> Int -> [Int]
mask2 bm n = flip mask n <$> bm

readBitmask :: String -> Bitmask
readBitmask = uncurry Bitmask . foldl f (0,0) where
 f (mask, value) char =
    let (bm, bv) = case char of { 'X' -> (1, 0); '0' -> (0, 0); '1' -> (0, 1); }
    in (mask * 2 + bm, value * 2 + bv)

readBitmask2 :: String -> [Bitmask]
readBitmask2 = (uncurry Bitmask <$>) . foldl f [(0, 0)] where
 f bms char =
    do (mask, value) <- bms
       (bm, bv) <- case char of
         'X' -> [(0, 0), (0, 1)] 
         '0' -> [(1, 0)]
         '1' -> [(0, 1)]
       return (mask * 2 + bm, value * 2 + bv)

step :: ([(Int, Int)], Bitmask) -> Instruction -> ([(Int, Int)], Bitmask)
step (mem, _) (SetBitmask bm) = (mem, readBitmask bm)
step (mem, bm) (SetMemoryValue key val) = ((key, mask bm val) : mem, bm)

step2 :: ([(Int, Int)], [Bitmask]) -> Instruction -> ([(Int, Int)], [Bitmask])
step2 (mem, _) (SetBitmask bm) = (mem, readBitmask2 bm)
step2 (mem, bm) (SetMemoryValue key val) = (((\b -> (mask b key, val)) <$> bm) ++ mem, bm)

run :: [Instruction] -> M.Map Int Int
run instrs = let (memory, _) = foldl step ([], Bitmask 0 0) instrs in
  M.fromListWith (const id) memory

run2 :: [Instruction] -> M.Map Int Int
run2 instrs = let (memory, _) = foldl step2 ([], []) instrs in
   M.fromListWith (const id) memory

parseProgram :: P.Parsec String () [Instruction]
parseProgram = instruction `P.endBy` P.newline where
  instruction =     (P.try (P.string "mask") >> mask  )
              P.<|> (P.try (P.string "mem" ) >> memory)
  mask = P.string " = " >> SetBitmask <$> P.many1 (P.noneOf ['\n'])
  memory = SetMemoryValue <$ P.char '[' <*> number <* P.string "] = " <*> number
  number = read <$> P.many1 P.digit

main :: IO ()
main = parseFromFile parseProgram "ex14/input.txt" >>= \case
  Right instrs -> do
    print instrs
    print . sum . M.elems $ run instrs
    print . sum . M.elems $ run2 instrs
  Left err -> print err

