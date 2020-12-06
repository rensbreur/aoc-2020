{-# LANGUAGE LambdaCase #-}

module Main where

import Text.Parsec ( char, digit, letter, newline, space, many1, many, Parsec ) 
import Text.Parsec.String ( parseFromFile )

data Policy = Policy Int Int Char

-- Parsing

number :: Parsec String () Int
number = read <$> many1 digit

policy :: Parsec String () Policy
policy = Policy <$> number <* char '-' <*> number <* space <*> letter <* char ':'

entry :: Parsec String () (Policy, String)
entry = (,) <$> policy <*> (space *> many1 letter)

pwList :: Parsec String () [(Policy, String)]
pwList = many (entry <* newline)

-- Validation

valid1 :: Policy -> String -> Bool
valid1 (Policy a b chr) pw = inRange . length $ filter (== chr) pw where
  inRange n = a <= n && n <= b

valid2 :: Policy -> String -> Bool
valid2 (Policy a b chr) pw = length pw >= b
                            && (pw!!(a-1) == chr) /= (pw!!(b-1) == chr)

-- Main

main :: IO ()
main = parseFromFile pwList "ex02/input.txt" >>= \case
  Right pws -> do
    let valids1 = filter (uncurry valid1) pws
    putStrLn $ "Answer 1: " ++ show (length valids1)
    let valids2 = filter (uncurry valid2) pws
    putStrLn $ "Answer 2: " ++ show (length valids2)
  Left err -> do
    putStrLn $ "Could not parse input: " ++ show err

