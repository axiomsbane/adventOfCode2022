#!/usr/bin/env cabal
{- cabal:
build-depends: base
            , split, containers
-}

import Data.List.Split (splitOn, chunksOf)
import Data.List (sort)
import Data.Char (ord, isUpper, isLower)
import qualified Data.Set as S

strToInt :: String -> Int
strToInt = read

oA = ord 'A' - 1
oa = ord 'a' - 1

getVal :: Char -> Int
getVal c 
    | isUpper c = ord c - oA + 26
    | isLower c = ord c - oa
    | otherwise = 0

getSet :: [Char] -> S.Set Char
getSet = S.fromList

solve :: ([Char], [Char]) -> Int
solve (x, y) = sum $ S.toList $ S.map getVal (S.fromList x `S.intersection` S.fromList y)

solve2 :: [[Char]] -> Int
solve2 lis = sum $ S.toList $ S.map getVal $ foldr1 S.intersection $ map S.fromList lis

main = do
    inpTxt <- readFile "input.txt"
    let splitTxt = map (\x -> splitAt (length x `div` 2) x) $ lines inpTxt
        ans1 = sum $ map solve splitTxt
        threeChunks = chunksOf 3 $ lines inpTxt
        ans2 = sum $ map solve2 threeChunks
    print ans1 
    print ans2

