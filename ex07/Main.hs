{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.String (parseFromFile)

type Color = String

type Regulations = M.Map Color [(Integer, Color)]

regulations :: Parsec String () Regulations
regulations = M.fromList <$> regulation `endBy` newline where
  regulation = (,) <$> bag <* string " contain " <*> (emptyBag <|> bags) <* char '.'
  bag = manyTill anyChar (try $ string " bag" >> optional (char 's'))
  emptyBag = [] <$ string "no other bags"
  bags = ((,) <$> number <* char ' ' <*> bag) `sepBy` string ", "
  number = read <$> many1 digit

containedIn :: Regulations -> Color -> [Color]
containedIn regs clr = M.keys . M.filter (isJust . find ((== clr) . snd)) $ regs

outer :: Regulations -> Color -> S.Set Color
outer regs clr = S.insert clr (S.unions $ outer regs <$> containedIn regs clr)

countInner :: Regulations -> Color -> Integer
countInner regs clr = sum . map (\(nr, clr) -> nr + nr * countInner regs clr) $ regs M.! clr

main :: IO ()
main = parseFromFile regulations "ex07/input.txt" >>= \case
  Right regulations -> do
    print . length $ outer regulations "shiny gold"
    print $ countInner regulations "shiny gold"
  Left err -> print err

