#!/usr/bin/env stack
{- stack script
   --resolver lts-19.28
   --package base,split,extra,containers,array
-}
{-# LANGUAGE  FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

import Control.Arrow ((<<<))
import Data.Char (digitToInt)
import Data.List (transpose,span)
import Data.Array 

type Idx = (Int, Int)
type Arr = Array Idx Int

maxBuilder :: Int -> [Int] -> [Int]
maxBuilder _ [] = []
maxBuilder mx (x:xs) = max x mx : maxBuilder (max x mx) xs

solv :: Int -> Int -> Arr -> Arr -> Arr -> Arr -> Arr -> Idx -> Bool
solv rLim cLim trees l r u d p@(x,y) 
    | x == 1 || x == rLim || y == 1 || y == cLim = True
    | otherwise = or $ map (< (trees ! p)) [l!pl,r!pr,u!pu,d!pd]
    where 
        pl = (x,y-1)
        pr = (x,y+1)
        pu = (x-1,y)
        pd = (x+1,y)

solv2 :: Int -> Int -> Arr -> Idx -> Int
solv2 rLim cLim trees p@(x,y) = 
    product [
        takeUntil cur 1 (map (x,) dl) trees,
        takeUntil cur 1 (map (x,) dr) trees,
        takeUntil cur 1 (map (,y) du) trees,
        takeUntil cur 1 (map (,y) dd) trees
    ]
    where 
        cur = trees!p
        [dl,dr,du,dd] = [reverse [1..(y-1)],[(y+1)..cLim],reverse [1..(x-1)],[(x+1)..rLim]] 

-- curnt val -> ctr -> idxs -> array
takeUntil :: Int -> Int -> [Idx] -> Arr -> Int
takeUntil _ ctr [] _ = (ctr-1)
takeUntil val ctr (x:xs) arr 
    | arr!x >= val = ctr
    | otherwise = takeUntil val (1+ctr) xs arr

main = do 
    inp <- readFile "input.txt"
    let leftRight = (map . map) digitToInt $ lines inp
        leftMaxLis = map (maxBuilder (-1)) leftRight
        topDown = transpose leftRight
        topMaxLis = transpose $ map (maxBuilder (-1)) topDown
        rightMaxLis = map (reverse . maxBuilder (-1) . reverse) leftRight
        bottomMaxlis = transpose $ map (reverse . maxBuilder (-1) . reverse) topDown 
        (r, c) = (length leftRight, length $ head leftRight)
        inds = [(i,j) | i <- [1..r], j <- [1..c]]
        fInds = filter (\(a,b) -> not(a==1||a==r||b==1||b==c)) inds
        lef = array ((1,1), (r, c)) $ zip inds $ concat leftMaxLis
        rig = array ((1,1), (r, c)) $ zip inds $ concat rightMaxLis
        top = array ((1,1), (r, c)) $ zip inds $ concat topMaxLis
        dow = array ((1,1), (r, c)) $ zip inds $ concat bottomMaxlis
        arr = array ((1,1), (r, c)) $ zip inds $ concat leftRight
        solv1 = map (solv r c arr lef rig top dow) inds
        ans2 = map (solv2 r c arr) fInds
    print $ length $ filter (==True) solv1 -- ans1
    print $ maximum ans2 -- ans2


    