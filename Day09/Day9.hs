#!/usr/bin/env stack
{- stack script
   --resolver lts-19.28
   --package base,split,extra,containers,array
-}
{-# LANGUAGE InstanceSigs,
GeneralizedNewtypeDeriving #-}
import Data.Containers.ListUtils (nubOrd)
import Data.List (splitAt)

newtype Pos = Pos {pos :: (Int, Int)}
    deriving (Show,Eq,Ord)

instance Num Pos where 
    (+) (Pos (x,y)) (Pos (w,z)) = Pos (x+w,y+z)
    (-) (Pos (x,y)) (Pos (w,z)) = Pos (x-w,y-z)
    abs (Pos (x,y)) = Pos (abs x, abs y)

zerPos = Pos (0,0)
rope1 = replicate 2 zerPos
rope2 = replicate 10 zerPos

sToI :: String -> Int
sToI = read

parseIns :: String -> [Pos] 
parseIns str = case str of 
    ('L' : ' ' : x) -> replicate (sToI x) (Pos (-1,0))
    ('R' : ' ' : x) -> replicate (sToI x) (Pos (1,0))
    ('U' : ' ' : x) -> replicate (sToI x) (Pos (0,1))
    ('D' : ' ' : x) -> replicate (sToI x) (Pos (0,-1))
    _               -> []

moveTail :: Pos -> Pos -> Pos
moveTail headPos tailPos
    | chebyDist <= 1 = tailPos
    | otherwise = tailPos + Pos (signum dx, signum dy)
    where 
        (dx,dy) = pos (headPos - tailPos)
        chebyDist = max (abs dx) (abs dy)

updateRope :: [Pos] -> Pos -> [Pos]
updateRope rope move = scanl moveTail (headKnot + move) tailKnots
    where 
        (headKnot, tailKnots) = (head rope, tail rope)

solve :: [Pos] -> [Pos] -> Int
solve rope moves = length $ nubOrd
                    $ map last
                    $ scanl updateRope rope moves
main = do 
    inp <- readFile "input.txt"
    let input = lines inp
        ins = concatMap parseIns input 
        ans1 = solve rope1 ins
        ans2 = solve rope2 ins
    print ans1
    print ans2