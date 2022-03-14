module Algorithm (
    MinPenListInfo(MinPenListInfo),
    findMin,
    getList,
    getPen
)
where

-- import Distribution.Simple.Utils (xargs)
import ConstraintConstr

-- define a data type to store the minimal penalty solution and its penalty
data MinPenListInfo = MinPenListInfo [Int] Int

-- to get the minimal penalty solution and its penalty
getList :: MinPenListInfo -> [Int]
getList (MinPenListInfo x _)= x
getPen :: MinPenListInfo -> Int
getPen (MinPenListInfo _ x) = x

-- input the constraints and the initial minPenInfo
-- output the solution and its penalty
findMin :: Constraint -> MinPenListInfo -> MinPenListInfo
findMin constraints minInfo = algo constraints (permutations [1..8]) minInfo

-- input the constraints, all possible solutions (permutations of [1..8]), and inital MinPenInfo
-- output the solution and its penalty
algo :: Constraint -> [[Int]] -> MinPenListInfo -> MinPenListInfo
algo constraints (permu:remainPermu) minPenInfo
    | null remainPermu = minPenInfo
    | checkHardConstraints constraints permu = algo constraints remainPermu (calMinList constraints permu minPenInfo)
    | otherwise = algo constraints remainPermu minPenInfo

checkHardConstraints :: Constraint -> [Int] -> Bool
checkHardConstraints constraints permu
    | permu /= [] = True -- check if the input permutation obeys all the hard constraints
    | otherwise = False

-- to check whether the input assignment passes the forced partial assignment
-- input [Int] for forced assignment constraint, and [Int] for current assignemnt
-- return True if pass the forced partial assignment, otherwise, return false
passesForcedAssignment :: (Eq a, Num a) => [a] -> [a] -> Bool
passesForcedAssignment [] [] = True
passesForcedAssignment [] (y:ys) = False
passesForcedAssignment (x:xs) [] = False
passesForcedAssignment (x:xs) (y:ys) 
    | x == -1 || x==y = passesForcedAssignment xs ys
    | otherwise = False

-- to check whether the input assignment passes the forbidden assignment
-- input [[Bool]] 8x8 array for forbidden assignment constraint, and [Int] for current assignemnt
-- return True if pass the forced partial assignment, otherwise, return false
passesForbiddenMachine :: [[Bool]] -> [Int] -> Bool
passesForbiddenMachine [] [] = True
passesForbiddenMachine (first_row : rest)  (x : xs) 
    | first_row !! x == False = False
    | otherwise = True && passesForbiddenMachine rest xs

-- --------------------------------------------------------------------------------------------------------------------

-- passesTooNearMach needed to be completed

-- --------------------------------------------------------------------------------------------------------------------

calMinList :: Constraint -> [Int] -> MinPenListInfo -> MinPenListInfo
calMinList constraints permu (MinPenListInfo minList minPen)
    | penalty (permu,constraints,0) < minPen = MinPenListInfo permu (penalty (permu,constraints,0))
    | otherwise = MinPenListInfo minList minPen

-- input a list
-- output all permutations of the input list with same type
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x : xs) = [zs | ys <- permutations xs, zs <- interleave x ys]

-- input a list
-- output a list from empty list to all element list (shift from start to end)
prefix :: [a] -> [[a]]
prefix [] = [[]]
prefix (x : xs) = [] : [x : y | y <- prefix xs]

-- input a list
-- output a list from all element list to empty list (shift from end to start)
suffix :: [a] -> [[a]]
suffix [] = [[]]
suffix (x :xs) = (x : xs) : suffix xs

-- input an element, and a list
-- output a list that showing all possible that the new element can be inserted
interleave :: a -> [a] -> [[a]]
interleave x xs = [ys ++ [x]++ zs | (ys, zs) <- zip (prefix xs) (suffix xs)]

-- input a task list ([0,...,7]), and all constraints (in Constraint data type)
-- output its penalty (too near penalty + machine penalty)
penalty :: ([Int], Constraint, Int) -> Int
penalty ([x], content, mach) = getMachPenalty content!!mach!!x
penalty (x:xs, content, mach)
    -- add too near penalties of position 0 and 1, and position 7 and 0 when the first time run the function  
    | mach == 0 = getMachPenalty content!!mach!!x + getNearPenalty content!!mach!!head xs + getNearPenalty content!!last xs!!x + penalty (xs, content, mach+1)
    -- add too near penalty of current position and next position 
    | otherwise      = getMachPenalty content!!mach!!x + getNearPenalty content!!x!!head xs + penalty (xs, content, mach+1)