#!/usr/bin/env stack
{- stack script
   --resolver lts-19.28
   --package base,split,extra,containers,array
-}
{-# LANGUAGE InstanceSigs,
GeneralizedNewtypeDeriving,
FlexibleInstances #-}
{-# LANGUAGE StrictData #-}

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.List (foldl',sort)
import Control.Arrow ((<<<))

type Items = [Int]

instance Show (Int -> Int) where
    show fn = show $ fn 7

instance Show (Int -> Bool) where
    show :: (Int -> Bool) -> String
    show fn = show $ fn 23

sToI :: String -> Int
sToI = read

data Monkey = Monkey {items :: !Items
                    , worryChange :: !(Int -> Int) 
                    , testFunc :: !(Int -> Bool)
                    , testNum :: !Int
                    , trueMonkey :: !Int
                    , falseMonkey :: !Int}
                deriving (Show)

defMonke = Monkey [] (const (-20)) (const True) 1 1 1

parseLis :: [String -> String]
parseLis = [(last . splitOn "s: "), (last .splitOn "ld ")] ++ replicate 3 (last . words)

getWorryFunc :: String -> (Int -> Int)
getWorryFunc str = case words str of 
    ["*", "old"] -> (\x -> x*x)
    ["+", "old"] -> (2 *)
    ["+", num]   -> (+ read num)
    ["*", num]   -> (* read num) 
    _anyOthFail  -> id

createMonkey :: [String] -> Monkey
createMonkey [things, worryChange, divNum, truMonke, falseMonke] =
    Monkey  (map sToI $ splitOn ", " things) 
            (getWorryFunc worryChange) 
            (\cur -> cur `mod` sToI divNum == 0) 
            (read divNum)
            (sToI truMonke) 
            (sToI falseMonke)
createMonkey _ = defMonke

unjustMonkey :: Maybe Monkey -> Monkey 
unjustMonkey Nothing = defMonke
unjustMonkey (Just monk) = monk

updateTruFalMonkeys :: [Monkey] -> [(Int, Bool)] -> [Monkey]
updateTruFalMonkeys monks [] = monks 
updateTruFalMonkeys [truM, falM] ((thing,cond):rem) = case cond of 
    True  -> updateTruFalMonkeys [truM {items = thing: items truM}, falM] rem
    False -> updateTruFalMonkeys [truM, falM {items = thing : items falM}] rem
updateTruFalMonkeys _ _ = [defMonke]

doIthMonkeyActivity :: (M.Map Int Int, M.Map Int Monkey) -> Int -> (M.Map Int Int, M.Map Int Monkey)
doIthMonkeyActivity (inspectCtr,monkeyMap) idx = (M.adjust (+thingSz) idx inspectCtr, M.insert idx updCur 
                                                    <<< M.insert iTru updTru 
                                                    <<< M.insert iFal updFal $ monkeyMap)
    where 
        curMonke = unjustMonkey $ M.lookup idx monkeyMap
        things = items curMonke
        thingSz = length things
        --updThings = map ((`div` testNum curMonke) .  (worryChange curMonke)) things 
        updThings = map ((`div` 3) . (worryChange curMonke)) things 
        usePairs = zip updThings $ map (testFunc curMonke) updThings
        [iTru, iFal] = [trueMonkey curMonke, falseMonkey curMonke]
        trufalMonks = map unjustMonkey [M.lookup iTru monkeyMap, M.lookup iFal monkeyMap]                          
        [updTru, updFal] = updateTruFalMonkeys trufalMonks usePairs
        updCur = curMonke {items = []}

do1Round :: Int -> (M.Map Int Int, M.Map Int Monkey) -> (M.Map Int Int, M.Map Int Monkey)
do1Round lim togMap = foldl' doIthMonkeyActivity togMap [0..lim]

doSolve :: Int -> Int -> (M.Map Int Int, M.Map Int Monkey) -> (M.Map Int Int, M.Map Int Monkey)
doSolve 0 _ mp = mp
doSolve x lim mp = doSolve (x-1) lim $ do1Round lim mp

main = do 
    inp <- readFile "input.txt"
    let sepInp = map (tail . lines) $ splitOn "\n\n" inp
        cleanInp = map (zipWith ($) parseLis) sepInp
        monkeys = map createMonkey cleanInp
        monkeyMap = M.fromList $ zip [0..] monkeys
        lim = length monkeys - 1
        inspectCtr = M.fromList $ zip [0..lim] [0,0..]
        togMap = (inspectCtr, monkeyMap)
        ansTmp1 = sort $ map snd $ M.toList $ fst $ doSolve 20 lim togMap
        finAns1 = product $ drop (lim - 1) ansTmp1
    print finAns1

