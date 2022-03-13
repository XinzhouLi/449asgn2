tooNearConstr :: [String] -> [[Bool]]
tooNearConstr _ = tooNearConvert [[]]
-- tooNearConstr (x:xs) = [[]]

tooNearConvert :: [String] -> [[Bool]] -> [[Bool]]
tooNearConvert _ x = x cycle 8 []


--machine Pens 8*8 matrix builder
machinePensConstr :: [String] ->  [[Int]] -> [[Int]]
machinePensConstr [x] matrix = matrix ++ [listNumConver (words x) []]
machinePensConstr (x:xs) matrix = machinePensConstr xs (matrix ++ [listNumConver (words x) []])


listNumConver :: [String] -> [Int] -> [Int]
listNumConver [x] intList = intList ++ [read x + 0]
listNumConver (x:xs) intList = listNumConver xs (intList ++ [read x + 0])
