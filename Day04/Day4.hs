
#!/usr/bin/env cabal
{- cabal:
build-depends: base
            , split
-}

import Data.List.Split (splitOn)

strToInt :: String -> Int
strToInt = read

pred1 :: [[Int]] -> Bool
pred1 [[x1, y1], [x2, y2]] = (x1 >= x2 && y1 <= y2) || (x1 <= x2 && y1 >= y2)
pred1 _ = False

pred2 :: [[Int]] -> Bool
pred2 [[x1, y1], [x2, y2]] = not (y1 < x2 || y2 < x1)

main = do
    inp <- readFile "input.txt"
    let linedInp = lines inp
        useInpTemp = map (splitOn ",") linedInp
        useInp = (map . map) (splitOn "-") useInpTemp
        useInpInt = (map . map . map) strToInt useInp
        ans1 = length $ filter pred1 useInpInt
        ans2 = length $ filter pred2 useInpInt
    print ans2