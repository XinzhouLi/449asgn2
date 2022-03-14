{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module ConstraintConstr (
    Constraint(Constraint),
    getForcedPartial,
    getForbidden,
    getNearTask,
    getMachPenalty,
    getNearPenalty,
    forcedPartialConstr,
    tooNearConstr,
    forbiddenConstr,
    machinePensConstr,
    constraintConstructor
)
where

import FileIO

-- define a data type to store all constraints input from file
data Constraint = Constraint [Int] [[Bool]] [[Bool]] [[Int]] [[Int]]

-- define functions to get constraints from Constraint data type
getForcedPartial :: Constraint -> [Int]
getForcedPartial (Constraint forcedPartial _ _ _ _) = forcedPartial
getForbidden :: Constraint -> [[Bool]]
getForbidden (Constraint _ forbidden _ _ _) = forbidden
getNearTask :: Constraint -> [[Bool]]
getNearTask (Constraint _ _ nearTask _ _) = nearTask
getMachPenalty :: Constraint -> [[Int]]
getMachPenalty (Constraint _ _ _ machPenalty _) = machPenalty
getNearPenalty :: Constraint -> [[Int]]
getNearPenalty (Constraint _ _ _ _ nearPenalty) = nearPenalty

-- input forced partial assignment
-- output 1x8 Int array
-- return [0,0,0,0,0,0,0,0] if there is "partial assignment error"
forcedPartialConstr :: [String] -> [Int]
forcedPartialConstr x = forcedPartialConvert x (take 8 (cycle [-1]))

forcedPartialConvert :: [String] -> [Int] -> [Int]
forcedPartialConvert [] list = list
forcedPartialConvert [[]] list = list
forcedPartialConvert (x:xs) [x1,x2,x3,x4,x5,x6,x7,x8] 
    | num == 0 && x1 == -1 = if (elem letter [x1,x2,x3,x4,x5,x6,x7,x8]) 
                                then [0,0,0,0,0,0,0,0]
                                else forcedPartialConvert xs [letter,x2,x3,x4,x5,x6,x7,x8]
    | num == 0 && x1 /= -1 = [0,0,0,0,0,0,0,0]
    | num == 1 && x2 == -1 = if (elem letter [x1,x2,x3,x4,x5,x6,x7,x8]) 
                                then [0,0,0,0,0,0,0,0]
                                else forcedPartialConvert xs [x1,letter,x3,x4,x5,x6,x7,x8]
    | num == 1 && x2 /= -1 = [0,0,0,0,0,0,0,0]
    | num == 2 && x3 == -1 = if (elem letter [x1,x2,x3,x4,x5,x6,x7,x8]) 
                                then [0,0,0,0,0,0,0,0]
                                else forcedPartialConvert xs [x1,x2,letter,x4,x5,x6,x7,x8]
    | num == 2 && x3 /= -1 = [0,0,0,0,0,0,0,0]
    | num == 3 && x4 == -1 = if (elem letter [x1,x2,x3,x4,x5,x6,x7,x8]) 
                                then [0,0,0,0,0,0,0,0]
                                else forcedPartialConvert xs [x1,x2,x3,letter,x5,x6,x7,x8]
    | num == 3 && x4 /= -1 = [0,0,0,0,0,0,0,0]
    | num == 4 && x5 == -1 = if (elem letter [x1,x2,x3,x4,x5,x6,x7,x8]) 
                                then [0,0,0,0,0,0,0,0]
                                else forcedPartialConvert xs [x1,x2,x3,x4,letter,x6,x7,x8]
    | num == 4 && x5 /= -1 = [0,0,0,0,0,0,0,0]
    | num == 5 && x6 == -1 = if (elem letter [x1,x2,x3,x4,x5,x6,x7,x8]) 
                                then [0,0,0,0,0,0,0,0]
                                else forcedPartialConvert xs [x1,x2,x3,x4,x5,letter,x7,x8]
    | num == 5 && x6 /= -1 = [0,0,0,0,0,0,0,0]
    | num == 6 && x7 == -1 = if (elem letter [x1,x2,x3,x4,x5,x6,x7,x8]) 
                                then [0,0,0,0,0,0,0,0]
                                else forcedPartialConvert xs [x1,x2,x3,x4,x5,x6,letter,x8]
    | num == 6 && x7 /= -1 = [0,0,0,0,0,0,0,0]
    | num == 7 && x8 == -1 = if (elem letter [x1,x2,x3,x4,x5,x6,x7,x8]) 
                                then [0,0,0,0,0,0,0,0]
                                else forcedPartialConvert xs [x1,x2,x3,x4,x5,x6,x7,letter]
    | num == 7 && x8 /= -1 = [0,0,0,0,0,0,0,0]
    where num = (convNumToInt (x!!1))
          letter = convLetToInt (x!!3)

-- input too near machine
-- output 8x8 boolean arrray ((x,y) is True if (x,y) is a too near machine, otherwise if False)
tooNearConstr :: [String] -> [[Bool]]
tooNearConstr x = tooNearConvert x (take 8 (cycle [(take 8 (cycle [False]))]))

tooNearConvert :: [String] -> [[Bool]] -> [[Bool]]
tooNearConvert (l:ls) x
    | l  == [] = x
    | ls == [] = arrayBoolChange (convLetToInt (l!!1)) (convLetToInt (l!!3)) 0 x
    | otherwise = tooNearConvert ls (arrayBoolChange (convLetToInt (l!!1)) (convLetToInt (l!!3)) 0 x)

-- input forbidden assignment
-- output 8x8 boolean arrray ((x,y) is False if (x,y) is a forbidden assignment, otherwise if True)
forbiddenConstr :: [String] -> [[Bool]]
forbiddenConstr x = forbiddenConvert x (take 8 (cycle [(take 8 (cycle [True]))]))

forbiddenConvert :: [String] -> [[Bool]] -> [[Bool]]
forbiddenConvert (l:ls) x
    | l  == [] = x
    | ls == [] = arrayBoolChangeForbid (convNumToInt (l!!1)) (convLetToInt (l!!3)) 0 x
    | otherwise = forbiddenConvert ls (arrayBoolChangeForbid (convNumToInt (l!!1)) (convLetToInt (l!!3)) 0 x)

-- input x, y, current X, 8x8 Boolean array
-- output (x,y)=True, 8x8 Boolean array
arrayBoolChange :: Int -> Int -> Int -> [[Bool]] -> [[Bool]]
arrayBoolChange x y curX [] = []
arrayBoolChange x y curX (l:ls)
    | ls == [] && curX == x = [arrayBoolFindY y 0 l]
    | ls == [] = [l]
    | curX == x = arrayBoolFindY y 0 l : arrayBoolChange x y (curX+1) ls
    | otherwise = l: arrayBoolChange x y (curX+1) ls  
arrayBoolFindY :: Int -> Int -> [Bool] -> [Bool]
arrayBoolFindY y curY (l:ls) 
    | ls == [] && curY == y = [True]
    | ls == [] = [False]
    | curY == y = True : arrayBoolFindY y (curY+1) ls
    | otherwise = l : arrayBoolFindY y (curY+1) ls

arrayBoolChangeForbid :: Int -> Int -> Int -> [[Bool]] -> [[Bool]]
arrayBoolChangeForbid x y curX [] = []
arrayBoolChangeForbid x y curX (l:ls)
    | ls == [] && curX == x = [arrayBoolFindYforbid y 0 l]
    | ls == [] = [l]
    | curX == x = arrayBoolFindYforbid y 0 l : arrayBoolChangeForbid x y (curX+1) ls
    | otherwise = l: arrayBoolChangeForbid x y (curX+1) ls  

arrayBoolFindYforbid :: Int -> Int -> [Bool] -> [Bool]
arrayBoolFindYforbid y curY (l:ls) 
    | ls == [] && curY == y = [False]
    | ls == [] = [True]
    | curY == y = False : arrayBoolFindYforbid y (curY+1) ls
    | otherwise = l : arrayBoolFindYforbid y (curY+1) ls

getSingleTooNear :: String -> [Int]
getSingleTooNear (x : ',' : y : ',' : pen) =  convLetToInt x : convLetToInt y : [read pen + 0]

-- too near penality constructor 
-- input string out put 8*8 matrix 
tooNearPenConstr :: [String] -> [[Int]]
tooNearPenConstr x = tooNearPenConvert x (take 8 (cycle [(take 8 (cycle [0]))]))

tooNearPenConvert :: [String] -> [[Int]] -> [[Int]]
tooNearPenConvert (l:ls) x
    | l  == [] = x
    | ls == [] = arrayIntChange (convLetToInt (l!!1)) (convLetToInt (l!!3)) ((getSingleTooNear (sliceList 0 ((length l)-1) l)) !! 2 ) 0 x
    | otherwise = tooNearPenConvert ls (arrayIntChange (convLetToInt (l!!1)) (convLetToInt (l!!3)) ((getSingleTooNear (sliceList 0 ((length l)-1) l)) !! 2 ) 0 x)

arrayIntChange :: Int -> Int -> Int -> Int -> [[Int]] -> [[Int]]
arrayIntChange x y z curX [] = []
arrayIntChange x y z curX (l:ls)
    | ls == [] && curX == x = [arrayIntFindY y z 0 l]
    | ls == [] = [l]
    | curX == x = arrayIntFindY y z 0 l : arrayIntChange x y z (curX+1) ls
    | otherwise = l: arrayIntChange x y z (curX+1) ls  
arrayIntFindY :: Int -> Int -> Int -> [Int] -> [Int]
arrayIntFindY y z curY (l:ls) 
    | ls == [] && curY == y = [z]
    | ls == [] = [0]
    | curY == y = z : arrayIntFindY y z (curY+1) ls
    | otherwise = l : arrayIntFindY y z (curY+1) ls


--machine Pens 8*8 matrix builder
machinePensConstr :: [String] ->  [[Int]] -> [[Int]]
machinePensConstr [x] matrix = matrix ++ [listNumConver (words x) []]
machinePensConstr (x:xs) matrix = machinePensConstr xs (matrix ++ [listNumConver (words x) []])


listNumConver :: [String] -> [Int] -> [Int]
listNumConver [x] intList = intList ++ [read x + 0]
listNumConver (x:xs) intList = listNumConver xs (intList ++ [read x + 0])

--convert letter to integer
convLetToInt :: Char -> Int
convLetToInt 'A' = 0
convLetToInt 'B' = 1
convLetToInt 'C' = 2
convLetToInt 'D' = 3
convLetToInt 'E' = 4
convLetToInt 'F' = 5
convLetToInt 'G' = 6
convLetToInt 'H' = 7
convLetToInt x = -1

--convert number to integer
convNumToInt :: Char -> Int
convNumToInt '1' = 0
convNumToInt '2' = 1
convNumToInt '3' = 2
convNumToInt '4' = 3
convNumToInt '5' = 4
convNumToInt '6' = 5
convNumToInt '7' = 6
convNumToInt '8' = 7
convNumToInt x = -1


constraintConstructor :: [String] -> [String] -> [String] -> [String] -> [String] -> Constraint
constraintConstructor partialAsgn forbiddenMa toonearTasks machinePens toonearPens = Constraint (forcedPartialConstr partialAsgn) (forbiddenConstr forbiddenMa) (tooNearConstr toonearTasks) (machinePensConstr machinePens [])(tooNearPenConstr toonearPens)