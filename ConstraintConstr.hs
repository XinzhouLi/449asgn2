tooNearConstr :: [String] -> [[Bool]]
tooNearConstr _ = tooNearConvert [[]]
-- tooNearConstr (x:xs) = [[]]

tooNearConvert :: [String] -> [[Bool]] -> [[Bool]]
tooNearConvert _ x = x cycle 8 []