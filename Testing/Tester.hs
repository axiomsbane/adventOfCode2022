{-# LANGUAGE OverloadedStrings #-}

module Tester where
import qualified Data.Text as T
import Data.List (elem)


strToInt :: String -> Int
strToInt = read

getProd :: [Int] -> [Int] -> Int
getProd [] _ = 1
getProd (x:xs) ys
  | elem (2020 - x) ys = x * (2020 - x)
  | otherwise = getProd xs ys 

main = do 
  inputTxt <- readFile "input.txt"
  let numStrs = lines inputTxt
      nums = map strToInt numStrs
  print $ getProd nums nums