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
data Track = Track {head :: Pos, tail :: Pos}
    deriving (Show)

sToI :: String -> Int
sToI = read

instance Num Pos where 
    (+) (Pos (x,y)) (Pos (w,z)) = Pos (x+w,y+z)
    (-) (Pos (x,y)) (Pos (w,z)) = Pos (x-w,y-z)
    abs (Pos (x,y)) = Pos (abs x, abs y)

calc :: Track -> Track
calc trk@(Track he ta)
    | chebyDist <= 1 = trk
    | otherwise = Track he (ta + Pos (signum dx, signum dy))
    where 
        (dx,dy) = pos (he - ta)
        chebyDist = max (abs dx) (abs dy)

--HeadKnot -> tailKnots -> updates posis of knots
propRope :: Pos -> [Pos] -> [Pos]
propRope _ [] = []
propRope he (x:xs) = newTe : propRope newTe xs
    where 
        tmp = calc $ Track he x 
        newTe = tail tmp

simulate :: [Pos] -> Pos -> [Pos] -> [Pos]
simulate [] _ _ = []
simulate (mov:movs) curHe knots = (last updKnots) : simulate movs updHe updKnots
    where 
        updHe = curHe + mov
        updKnots = propRope updHe knots

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
        solvHelp2 = simulate ins zerPos rope2
        ans2 = size $ fromList solvHelp2
        ans1 = size $ fromList $ simulate ins zerPos rope1
    print ans1
    print ans2
