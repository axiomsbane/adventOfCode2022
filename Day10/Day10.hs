#!/usr/bin/env stack
{- stack script
   --resolver lts-19.28
   --package base,split,extra,containers,array
-}

import Data.List.Split (chunksOf)

sToI :: String -> Int
sToI = read

parseIns :: String -> [Int]
parseIns str = case str of 
    ('a':'d':'d':'x':' ':rem) -> [0, sToI rem]
    "noop"                    -> [0]
    _                         -> []

idxs :: [[a] -> a]
idxs = [(!! 19), (!! 59), (!! 99), (!! 139), (!! 179), (!! 219)]
inds = [20,60,100,140,180,220]

initPos = [1,2,3]

duringCycles = [0,1..239]

solve :: [Int] -> Int -> Char
solve op cyc = if (((cyc+1) `mod` 40) `elem` spriteSpan) then '#' else '.'
    where 
        afterCycInit = op !! cyc 
        spriteSpan = map ($ afterCycInit) [(+0),(+1),(+2)] 

main = do
    inp <- readFile "input.txt"
    let input = lines inp   
        ins = concatMap parseIns input
        outPut = scanl (+) 1 ins
        ans1 = sum $ zipWith (*) inds $ map ($ outPut) idxs
        charOp = map (solve outPut) duringCycles
        chru = chunksOf 40 charOp
    print ans1
    mapM putStrLn chru

