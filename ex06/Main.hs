{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List ( intersect, union )
import Text.Parsec
import Text.Parsec.String ( parseFromFile )

type Group = [Declaration]
type Declaration = [Char]

groups :: Parsec String () [Group]
groups = many1 lower `endBy` newline `sepBy` newline

decl1 :: Group -> Declaration
decl1 = foldl1 union

decl2 :: Group -> Declaration
decl2 = foldl1 intersect

main :: IO ()
main = parseFromFile groups "ex06/input.txt" >>= \case
  Right groups -> do
    putStrLn $ "Answer 1: " ++ show (sum $ length . decl1 <$> groups)
    putStrLn $ "Answer 2: " ++ show (sum $ length . decl2 <$> groups)
  Left err -> print err

