data Constraint = Constraint [Int] [[Bool]] [[Bool]] [[Int]] [[Int]]

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

penalty :: ([Int], [[Int]], Int) -> Int
penalty ([], _, _) = 0
penalty (x:xs, np, mach) = np!!mach!!x + penalty (xs, np, mach+1)

main = do
    
    let normalP = [[11,12,13],[21,22,23],[31,32,33]]
    let toonearPen = [[0,0,1],[1,0,0],[0,1,0]]
    let input = ["1,A"]
    let content = Constraint [0,1,-1] [[False,False,True],[True,False,False],[False,True,False]] [[False,False,True],[True,False,False],[False,False,False]] [[1,0,0],[0,2,0],[0,0,3]] [[0,2,0],[1,0,0],[0,0,0]]
    print(penalty ([0,2,1], normalP, 0))
    -- A, C, B
