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
rope1 = [zerPos]
rope2 = replicate 9 zerPos

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

simulate :: [Pos] -> Pos -> [Pos] -> [Pos]
simulate [] _ _ = []
simulate (move:moves) headPos knots = last updRope : simulate moves updHeadPos updRope
    where 
        updHeadPos = headPos + move
        updRope = tail $ scanl moveTail updHeadPos knots

-- Try to simplify simulate by converting into scanl 
-- not working rn for some reason
-- updateRope :: [Pos] -> Pos -> [Pos]
-- updateRope rope move = scanl moveTail (headKnot + move) tailKnots
--     where 
--         (headKnot, tailKnots) = (head rope, tail rope)

-- solve :: [Pos] -> [Pos] -> Int
-- solve rope moves = length $ nubOrd
--                     $ map last
--                     $ scanl updateRope rope moves
main = do 
    inp <- readFile "input.txt"
    let input = lines inp
        ins = concatMap parseIns input 
        ans1 = length $ nubOrd $ simulate ins zerPos rope1
        ans2 = length $ nubOrd $ simulate ins zerPos rope2
    print ans1
    print ans2