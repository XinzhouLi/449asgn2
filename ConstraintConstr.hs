module ConstraintConstr (
    forcedPartialConstr,
    tooNearConstr,
    forbiddenConstr,
    machinePensConstr
)
where

-- input forced partial assignment
-- output 1x8 Int array
forcedPartialConstr :: [String] -> [Int]
forcedPartialConstr x = forcedPartialConvert x (take 8 (cycle [-1]))

forcedPartialConvert :: [String] -> [Int] -> [Int]
forcedPartialConvert [] list = list
forcedPartialConvert (x:xs) [x1,x2,x3,x4,x5,x6,x7,x8] 
    | num == 0 = forcedPartialConvert xs [letter,x2,x3,x4,x5,x6,x7,x8]
    | num == 1 = forcedPartialConvert xs [x1,letter,x3,x4,x5,x6,x7,x8]
    | num == 2 = forcedPartialConvert xs [x1,x2,letter,x4,x5,x6,x7,x8]
    | num == 3 = forcedPartialConvert xs [x1,x2,x3,letter,x5,x6,x7,x8]
    | num == 4 = forcedPartialConvert xs [x1,x2,x3,x4,letter,x6,x7,x8]
    | num == 5 = forcedPartialConvert xs [x1,x2,x3,x4,x5,letter,x7,x8]
    | num == 6 = forcedPartialConvert xs [x1,x2,x3,x4,x5,x6,letter,x8]
    | num == 7 = forcedPartialConvert xs [x1,x2,x3,x4,x5,x6,x7,letter]

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

forbiddenConstr :: [String] -> [[Bool]]
forbiddenConstr x = forbiddenConvert x (take 8 (cycle [(take 8 (cycle [False]))]))

forbiddenConvert :: [String] -> [[Bool]] -> [[Bool]]
forbiddenConvert (l:ls) x
    | l  == [] = x
    | ls == [] = arrayBoolChange (convNumToInt (l!!1)) (convLetToInt (l!!3)) 0 x
    | otherwise = forbiddenConvert ls (arrayBoolChange (convNumToInt (l!!1)) (convLetToInt (l!!3)) 0 x)


-- input x, y, current X, 8x8 Boolean array
-- output (x,y)=True, 8x8 Boolean array
arrayBoolChange :: Int -> Int -> Int -> [[Bool]] -> [[Bool]]
arrayBoolChange x y curX (l:ls)
    | ls == [] && curX == x = [arrayBoolFindY y 0 l]
    | ls == [] = [l]
    | curX == x = arrayBoolFindY y 0 l : arrayBoolChange x y (curX+1) ls
arrayBoolFindY :: Int -> Int -> [Bool] -> [Bool]
arrayBoolFindY y curY (l:ls) 
    | ls == [] && curY == y = [True]
    | ls == [] = [False]
    | curY == y = True : arrayBoolFindY y (curY+1) ls
    | otherwise = l : arrayBoolFindY y (curY+1) ls



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
