#!/usr/bin/env stack
{- stack script
   --resolver lts-19.28
   --package base,split,extra,containers,array
-}
{-# LANGUAGE InstanceSigs,
GeneralizedNewtypeDeriving #-}
import Data.Set hiding (map)
import Prelude hiding (head,tail,sum)
import Data.List (nub)

type Vis = Set Pos
newtype Pos = Pos {pos :: (Int, Int)}
    deriving (Show,Eq,Ord)

sToI :: String -> Int
sToI = read

instance Num Pos where 
    (+) (Pos (x,y)) (Pos (w,z)) = Pos (x+w,y+z)
    (-) (Pos (x,y)) (Pos (w,z)) = Pos (x-w,y-z)
    abs (Pos (x,y)) = Pos (abs x, abs y)

updateRope :: Pos -> [Pos] -> [Pos]
updateRope _ [] = []
updateRope useHead (useTail:xs) = updTail : updateRope updTail xs
    where 
        updTail = moveTail useHead useTail

moveTail :: Pos -> Pos -> Pos
moveTail headPos tailPos
    | chebyDist <= 1 = tailPos
    | otherwise = tailPos + Pos (signum dx, signum dy)
    where 
        (dx,dy) = pos (headPos - tailPos)
        chebyDist = max (abs dx) (abs dy)

simulateClean :: [Pos] -> Pos -> [Pos] -> [Pos]
simulateClean [] _ _ = []
simulateClean (mov:movs) curHe knots = (last updKnots) : simulateClean movs updHe updKnots
    where 
        updHe = curHe + mov
        updKnots = updateRope updHe knots

zerPos = Pos (0,0)
rope1 = [zerPos]
rope2 = replicate 9 zerPos

getPos :: String -> [Pos] 
getPos ('L' : ' ' : x) = replicate (sToI x) (Pos (-1,0))
getPos ('R' : ' ' : x) = replicate (sToI x) (Pos (1,0))
getPos ('U' : ' ' : x) = replicate (sToI x) (Pos (0,1))
getPos ('D' : ' ' : x) = replicate (sToI x) (Pos (0,-1))
getPos _ = []

main = do 
    inp <- readFile "input.txt"
    let input = lines inp
        ins = concatMap getPos input 
        ans2Clean = size $ fromList $ simulateClean ins zerPos rope2
        ans1Clean = size $ fromList $ simulateClean ins zerPos rope1
    print ans1Clean
    print ans2Clean
