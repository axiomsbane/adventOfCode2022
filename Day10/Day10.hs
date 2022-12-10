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
inds = [20,60,100,140,180,220]
indsMinus1 = map (+ (-1)) inds

duringCycles = [0,1..239]

solve :: [Int] -> Int -> Char
solve registerVals cyc = if ((cyc+1) `mod` 40) `elem` spritePos then '#' else '.'
    where 
        spriteIdx1Pos = registerVals !! cyc 
        spritePos = map ($ spriteIdx1Pos) [(+0),(+1),(+2)]
main = do
    inp <- readFile "input.txt"
    let input = lines inp   
        ins = concatMap parseIns input
        outPut = scanl (+) 1 ins
        ans1 = sum $ zipWith (*) inds $ map (outPut !!) indsMinus1
        crtOutput = chunksOf 40 $ map (solve outPut) duringCycles
    print ans1
    mapM putStrLn crtOutput