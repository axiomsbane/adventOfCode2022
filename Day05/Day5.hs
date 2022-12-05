#!/usr/bin/env stack
{- stack script
   --resolver lts-19.28
   --package base,split,extra,containers
-}

import Data.List.Split (splitOn, chunksOf)
import Data.List.Extra (trim)
import Data.List (transpose)
import qualified Data.Map as DM 
import Data.Char (isUpper, isAlpha)

strToInt :: String -> Int
strToInt = read

simulate :: (DM.Map Int [String]) -> [Int] -> (DM.Map Int [String])
simulate mp [amt, frm, tu] = DM.insert frm remrem $ DM.insert tu attu mp
    where 
        (takeOut, remrem) = splitAt amt (mp DM.! frm)
        attu = (reverse takeOut) ++ (mp DM.! tu) 

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
        finCrate = transpose $ (map . map) trim nonTrim
        useCrate = map (filter (""/=)) finCrate
        mappu = DM.fromList $ zip [1..(length useCrate)] useCrate
        movesBoi = map (words . trim . filter (not .isAlpha)) moveUse
        movvu = (map . map) strToInt movesBoi
        finMap = foldl simulate mappu movvu
        getVals = (map (\x -> DM.lookup x finMap) [1..(length useCrate)])
        makeAns1 = filter (\x -> x /= '[' && x /= ']') $ concatMap jusDoIt getVals
    print makeAns1
    