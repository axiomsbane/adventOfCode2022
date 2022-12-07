#!/usr/bin/env stack
{- stack script
   --resolver lts-19.28
   --package base,split,extra,containers,array
-}

import Data.Containers.ListUtils (nubOrd)
globSz = 14 

sol :: [Char] -> Int -> Int
sol lis ctr = case globSz == (length . nubOrd $ (take globSz lis)) of
    True  -> (ctr + globSz - 1)
    False -> sol (tail lis) (1 + ctr)

main = do 
    inp <- readFile "input.txt"
    print $ sol inp 1