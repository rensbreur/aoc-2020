{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List (find)
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import Text.Parsec
import Text.Parsec.String ( parseFromFile )

data Instruction = Instruction Opcode Int
data Opcode = Nop | Jmp | Acc

-- allows for lazy patching
data Bootcode = Bootcode { length :: Int, (!) :: Int -> Instruction }

fromVector :: V.Vector Instruction -> Bootcode
fromVector v = Bootcode (V.length v) (v V.!)

data HandheldState = HandheldState { ip :: Int, acc :: Int }

initialState :: HandheldState
initialState = HandheldState 0 0

data ExecState = LoopExc | Finished | IndexExc

finished :: ExecState -> Bool
finished Finished = True
finished _        = False

-- Parsing 

bootcode :: Parsec String () Bootcode
bootcode = fromVector . V.fromList <$> instr `endBy` newline where
  instr = Instruction <$> opcode <* space <*> number
  opcode = (Nop <$ string "nop") <|> (Jmp <$ string "jmp") <|> (Acc <$ string "acc")
  number = (id <$ char '+' <|> negate <$ char '-') <*> (read <$> many1 digit)

-- Interpreting bootcode

step :: Bootcode -> HandheldState -> HandheldState
step code hs@HandheldState{ ip = ip, acc = acc } = case code Main.! ip of
  Instruction Nop _      -> hs{ ip = ip + 1 }
  Instruction Jmp offset -> hs{ ip = ip + offset }
  Instruction Acc add    -> hs{ ip = ip + 1, acc = acc + add }

continue :: Bootcode -> HandheldState -> (HandheldState, ExecState)
continue code hso = go hso [] where
  go hso instrs =
    let hs@HandheldState{ ip = ip } = step code hso in
    if ip `elem` instrs
      then (hs, LoopExc)
    else if ip >= Main.length code
      then (hs, Finished)
    else if ip < 0
      then (hs, IndexExc)
    else go hs (ip : instrs)

run :: Bootcode -> (HandheldState, ExecState)
run = (`continue` initialState)

accValue :: (HandheldState, ExecState) -> Int
accValue (state, _) = acc state

-- Program patching 

patch :: Bootcode -> Int -> Instruction -> Bootcode
patch code i p = Bootcode (Main.length code) $ \j -> if i == j then p else code ! j

patched :: Bootcode -> [Bootcode]
patched code = mapMaybe (\n -> patch code n <$> swap (code ! n) ) [0..(Main.length code)]
 where
   swap (Instruction opc d) = flip Instruction d <$> case opc of
    Jmp -> Just Nop
    Nop -> Just Jmp
    _   -> Nothing

repair :: Bootcode -> Maybe (HandheldState, ExecState) 
repair result = find (finished . snd) $ map run (patched result)

-- Main

main :: IO ()
main = parseFromFile bootcode "ex08/input.txt" >>= \case
  Right result -> do
    print $ "Answer 1: " ++ (show . accValue . run $ result)
    print $ "Answer 2: " ++ (show . fmap accValue . repair $ result)
  Left err -> print err

