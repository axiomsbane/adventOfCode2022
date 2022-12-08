#!/usr/bin/env stack
{- stack script
   --resolver lts-19.28
   --package base,split,extra,containers,array
-}
import Data.List (stripPrefix)
import Data.Char (isNumber)
import qualified Data.Map as M
import Control.Arrow ((<<<))
import Data.List (find,sort)

strToInt :: String -> Int
strToInt = read

isMatch :: String -> String -> (Bool, String)
isMatch pref str = case stripPrefix pref str of
    Nothing -> (False, "")
    Just x  -> (True, x) 

markPref = isMatch "$ cd "
passPref = [fst . isMatch "dir", fst . isMatch "$ ls"]

push :: [a] -> a -> [a]
push stk x = x : stk

pop :: [a] -> [a]
pop [] = []
pop (x:xs) = xs

mxSize = 100000

upd :: Int -> [String] -> (M.Map String Int) -> (M.Map String Int)
upd _ [] mp = mp
upd val (x:xs) mp = upd val xs (M.adjust (+ val) x mp)

-- UUID -> instructions -> trackingStack -> Directory Size tracker map -> final Map
solv :: Int -> [String] -> [String] -> (M.Map String Int) -> (M.Map String Int)
solv uuid (x:xs) stk szCtr 
    | isCd = if rem1 == ".." 
             then solv uuid xs (pop stk) szCtr
             else solv (1+uuid) xs (push stk rem) (M.insert rem 0 szCtr) 
    | b1 || b2 = solv uuid xs stk szCtr
    | otherwise = solv uuid xs stk <<< upd (strToInt $ takeWhile isNumber x) stk $ szCtr
    where   
        (isCd, rem1) = markPref x
        [b1, b2] = map ($ x) passPref
        rem = (show uuid) ++ rem1
solv _ [] _ szCtr = szCtr
req = 30000000
tot = 70000000

-- Hacky way to get out of Maybe in 
-- this problem scenario
utir :: Maybe Int -> Int 
utir (Just x) = x
utir Nothing = 0

fn :: Int -> [Int] -> Int
fn _ [] = 0
fn v (x:xs) = case (tot - (v - x) >= req) of 
    True -> x 
    False -> fn v xs 

main = do 
    inp <- readFile "input.txt"
    let input = lines inp
        finMap = solv 0 input [] M.empty
        lis = sort $ map snd $ M.toList finMap
        ans = sum $ filter (<= mxSize) lis
        curSz = utir $ M.lookup "0/" finMap 
    print ans
    print $ fn curSz lis
