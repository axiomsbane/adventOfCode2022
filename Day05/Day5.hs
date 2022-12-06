#!/usr/bin/env stack
{- stack script
   --resolver lts-19.28
   --package base,split,extra,containers
-}
{-# LANGUAGE  ScopedTypeVariables #-}

import Data.List.Split (splitOn, chunksOf)
import Data.List.Extra (trim)
import Data.List (transpose)
import qualified Data.Map as DM 
import Data.Char (isUpper, isAlpha)
import Control.Arrow ((>>>),(<<<))

strToInt :: String -> Int
strToInt = read

fn :: [String] -> [String]
-- fn = reverse -- for part 1
fn = id -- part 2

simulate :: ([String] -> [String]) -> (DM.Map Int [String]) -> [Int] -> (DM.Map Int [String])
simulate ff mp [amt, frm, tu] = DM.insert frm remrem $ DM.insert tu attu mp
    where 
        (takeOut, remrem) = splitAt amt (mp DM.! frm)
        attu = (ff takeOut) ++ (mp DM.! tu) 

jusDoIt :: Maybe [String] -> String
jusDoIt (Just []) = ""
jusDoIt (Just lis) = head lis
jusDoIt _ = ""

main = do 
    inp <- readFile "input.txt"
    let sprittu = splitOn "\n\n" inp
        crateUse = init $ lines $ head sprittu
        moveUse = lines (sprittu !! 1)
        nonTrim = map (chunksOf 4) crateUse 
        mappu = DM.fromList $ zip [1..9] 
                <<< map (filter (""/=)) 
                <<< transpose $ (map . map) trim nonTrim 
        movvu :: [[Int]] = (map . map) strToInt 
                <<< map (words . trim . filter (not .isAlpha)) $ moveUse
        finMap = foldl (simulate fn) mappu movvu
        makeAns1 = filter (\x -> x /= '[' && x /= ']') $ concatMap jusDoIt
                <<< map (\x -> DM.lookup x finMap) $ [1..9]
    print makeAns1
    