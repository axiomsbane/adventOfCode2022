#!/usr/bin/env cabal
{- cabal:
build-depends: base
            , split
-}

import Data.List.Split (splitOn)

singScore :: [String] -> Int
singScore [_, "X"] = 1
singScore [_, "Y"] = 2 
singScore [_, "Z"] = 3 
singScore _ = 0

drawLis = [["A", "X"], ["B", "Y"], ["C", "Z"]]
winLis = [["C", "X"], ["A", "Y"], ["B", "Z"]]
loseLis= [["B", "X"], ["C", "Y"], ["A", "Z"]]

playScore :: [String] -> Int
playScore lis 
    | lis `elem` drawLis = 3
    | lis `elem` winLis = 6
    | otherwise = 0 

combineScore :: [String] -> Int
combineScore arg = sum $ map ($ arg) [singScore, playScore]

intelliPlayScore :: [String] -> Int
intelliPlayScore [x, y] = case y of 
    "X" -> combineScore $ concat $ filter (x `elem`) loseLis
    "Y" -> combineScore $ concat $ filter (x `elem`) drawLis
    "Z" -> combineScore $ concat $ filter (x `elem`) winLis
    _ -> 0
intelliPlayScore _ = 0

main = do
    inputTxt <- readFile "input.txt"
    let lisoflis = map words $ lines inputTxt
    print $ sum $ map combineScore lisoflis
    print $ sum $ map intelliPlayScore lisoflis
