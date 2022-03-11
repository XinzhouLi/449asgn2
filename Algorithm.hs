{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Algorithm (
    MinPenListInfo(MinPenListInfo),
    algo,
    getList,
    getPen
)
where


import Penalty
import Distribution.Simple.Utils (xargs)
data MinPenListInfo = MinPenListInfo [Int] Int

getList :: MinPenListInfo -> [Int]
getList (MinPenListInfo x _)= x
getPen :: MinPenListInfo -> Int
getPen (MinPenListInfo _ x) = x


algo :: Constraint -> [[Int]] -> MinPenListInfo -> MinPenListInfo
algo constraints (permu:remainPermu) minPenInfo
    | null remainPermu = minPenInfo
    | checkHardConstraints constraints permu = algo constraints remainPermu (calMinList constraints permu minPenInfo)
    | otherwise = algo constraints remainPermu minPenInfo

checkHardConstraints :: Constraint -> [Int] -> Bool
checkHardConstraints constraints permu
    | permu /= [] = True -- check if the input permutation obeys all the hard constraints
    | otherwise = False

calMinList :: Constraint -> [Int] -> MinPenListInfo -> MinPenListInfo
calMinList constraints permu (MinPenListInfo minList minPen)
    | penalty (permu,constraints,0) < minPen = MinPenListInfo permu (penalty (permu,constraints,0))
    | otherwise = MinPenListInfo minList minPen
