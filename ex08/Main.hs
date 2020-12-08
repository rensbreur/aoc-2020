{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List (find)
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import Text.Parsec
import Text.Parsec.String ( parseFromFile )

data Instruction = Nop Int | Jmp Int | Acc Int
  deriving (Show)

data Bootcode = Bootcode { (!) :: Int -> Instruction, length :: Int }

fromVector :: V.Vector Instruction -> Bootcode
fromVector v = Bootcode (v V.!) (V.length v)

patch :: Bootcode -> Int -> Instruction -> Bootcode
patch code i p = Bootcode f (Main.length code) where
  f j | i == j    = p
      | otherwise = code Main.! j

swap :: Instruction -> Maybe Instruction
swap (Jmp n) = Just (Nop n)
swap (Nop n) = Just (Jmp n)
swap _       = Nothing

data HandheldState = HandheldState { ip :: Int, acc :: Int }
  deriving Show

data ExecState = LoopExc | Finished | IndexExc
  deriving Show

finished :: ExecState -> Bool
finished Finished = True
finished _        = False

bootcode :: Parsec String () Bootcode
bootcode = fromVector . V.fromList <$> instr `endBy` newline where
  instr   =   Nop <$> opc "nop"
          <|> Jmp <$> opc "jmp"
          <|> Acc <$> opc "acc"
  opc str =   try $ string str >> char ' ' >> number

number :: (Read a, Num a) => Parsec String () a
number = (id <$ char '+' <|> negate <$ char '-') <*> (read <$> many1 digit)

step :: Bootcode -> HandheldState -> HandheldState
step code hs@HandheldState{ ip = ip, acc = acc } = case code Main.! ip of
  Nop _      -> hs{ ip = ip + 1 }
  Jmp offset -> hs{ ip = ip + offset }
  Acc add    -> hs{ ip = ip + 1, acc = acc + add }

continue :: Bootcode -> HandheldState -> (HandheldState, ExecState)
continue code hso = go hso [] where
  go hso instrs =
    let hs@HandheldState{ ip = ip } = step code hso in
    if ip `Prelude.elem` instrs
      then (hs, LoopExc)
    else if ip >= Main.length code
      then (hs, Finished)
    else if ip < 0
      then (hs, IndexExc)
    else go hs (ip : instrs)

possiblePrograms :: Bootcode -> [Bootcode]
possiblePrograms code = Data.Maybe.mapMaybe (\n -> patch code n <$> swap (code Main.! n) ) [0..(Main.length code)]

main :: IO ()
main = parseFromFile bootcode "ex08/input.txt" >>= \case
  Right result -> do
    print $ continue result (HandheldState 0 0)
    print $ fmap fst $ find (finished . snd) $ map (`continue` HandheldState 0 0) (possiblePrograms result)
  Left err -> print err

