module Main where

import Data.List.Split (splitOn)
import Data.List (sort)

strToInt :: String -> Int
strToInt = read

main = do
  inputTxt <- readFile "input.txt"
  let sprittu = splitOn "\n\n" inputTxt
  let sep = map (sum . (map strToInt) . lines) sprittu
  print $ maximum sep
  print $ sum $ take 3 $ reverse $ sort sep
