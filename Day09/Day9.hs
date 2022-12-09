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
    deriving (Show)
data Track = Track {head :: Pos, tail :: Pos}
    deriving (Show)
sToI :: String -> Int
sToI = read

instance Eq Pos where
    (==) (Pos (x,y)) (Pos (z,w)) = x==z && y==w

instance Ord Pos where 
    compare :: Pos -> Pos -> Ordering
    compare (Pos (x,y)) (Pos (z,w))
        | x < y = LT
        | x == y = EQ
        | otherwise = GT

instance Num Pos where 
    (+) (Pos (x,y)) (Pos (w,z)) = Pos (x+w,y+z)
    (-) (Pos (x,y)) (Pos (w,z)) = Pos (x-w,y-z)
    abs (Pos (x,y)) = Pos (abs x, abs y)

sum :: (Int,Int) -> Int
sum (x,y) = x+y

-- hamm > 2 only will come here
upd :: Int -> Int
upd val 
    | val < -1 = -1
    | val > 1 = 1
    | otherwise = val

calc :: Track -> Track
calc trk@(Track he ta)
    | hammDist <= 2 && not ((dx==0 && (abs dy)==2) || ((abs dx)==2 && dy==0)) = trk
    | otherwise = Track he (ta + Pos (upd dx, upd dy))
    where 
        (dx,dy) = pos (he - ta)
        (taX,taY) = pos ta
        hammDist = sum $ pos $ abs $ (he - ta)

simulateHelp :: [Pos] -> Track -> [Track] -> [Track]
simulateHelp [] _ vis = []
simulateHelp (mov:movs) (Track he ta) mp = calTa : simulateHelp movs calTa mp
    where 
        calTa = calc (Track (he+mov) ta) 


--11111111111111111111111111111111111111111111111111111111111111111111111111111111111111
--11111111111111111111111111111111111111111111111111111111111111111111111111111111111111

--HeadKnot -> tailKnots -> updates posis of knots
propRope :: Pos -> [Pos] -> [Pos]
propRope _ [] = []
propRope he (x:xs) = newTe : propRope newTe xs
    where 
        tmp = calc $ Track he x 
        newTe = tail tmp

simulateHelp2 :: [Pos] -> Pos -> [Pos] -> [Pos] -> [Pos]
simulateHelp2 [] _ _ vis = []
simulateHelp2 (mov:movs) curHe knots mp = (last updKnots) : simulateHelp2 movs updHe updKnots mp
    where 
        updHe = curHe + mov
        updKnots = propRope updHe knots

getPos :: String -> [Pos] 
getPos ('L' : ' ' : x) = replicate (sToI x) (Pos (-1,0))
getPos ('R' : ' ' : x) = replicate (sToI x) (Pos (1,0))
getPos ('U' : ' ' : x) = replicate (sToI x) (Pos (0,1))
getPos ('D' : ' ' : x) = replicate (sToI x) (Pos (0,-1))
getPos _ = []

rope = replicate 9 zerPos
zerPos = Pos (0,0)
initialTrack = Track zerPos zerPos
main = do 
    inp <- readFile "input.txt"
    let input = lines inp
        ins = concatMap getPos input 
        solvHelp = simulateHelp ins initialTrack []
        ans1 = size $ fromList $ map (pos . tail) solvHelp
        solvHelp2 = simulateHelp2 ins zerPos rope []
        ans2 = size $ fromList solvHelp2
    print $ length $ nub solvHelp2
