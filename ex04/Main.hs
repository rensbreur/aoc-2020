{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (guard)
import Data.Map as Map (Map, keys, (!?), fromList)
import Data.Maybe (isJust)
import Text.Parsec

type Passport = Map String String

-- Parsing 

passport :: Parsec String () Passport
passport = Map.fromList <$> assoc `endBy` oneOf separators
  where assoc = (,) <$> many1 lower <* char ':' <*> many1 (noneOf separators)
        separators = [' ', '\n']

number :: Parsec String () Int
number = read <$> many1 digit

-- Validation 

valid1 :: Passport -> Bool
valid1 pp = all (`elem` keys pp) kys
  where kys = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

valid2 :: Passport -> Bool
valid2 pp =
  isJust $ do
    prs "byr" number >>= guard . inRange 1920 2002
    prs "iyr" number >>= guard . inRange 2010 2020 
    prs "eyr" number >>= guard . inRange 2020 2030 
    prs "hgt" ((,) <$> number <*> many1 lower) >>= guard . \case
      (i, "cm") -> inRange 150 193 i
      (i, "in") -> inRange  59  76 i
      _         -> False
    prs "hcl" $ char '#' *> count 6 (digit <|> oneOf "abcdef")
    prs "ecl" $ many1 letter >>= guard . (`elem` ["amb","blu","brn","gry","grn","hzl","oth"])
    prs "pid" $ count 9 digit 
  where
    prs key pVal = pp !? key >>= eitherToMaybe . parse (pVal <* eof) ""

-- Main

main :: IO ()
main = do
  input <- readFile "ex04/input.txt"
  case parse (passport `sepBy` newline) "" input of
    Right result -> do
      putStrLn $ "Answer 1: " ++ show (length $ filter valid1 result)
      putStrLn $ "Answer 2: " ++ show (length $ filter valid2 result)
    Left error -> print error

-- Utilities

inRange :: Int -> Int -> Int -> Bool
inRange l u n = l <= n && n <= u 

eitherToMaybe :: Either a b -> Maybe b 
eitherToMaybe = either (const Nothing) Just 

