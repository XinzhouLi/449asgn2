module Penalty (
    Constraint(Constraint),
    penalty
) where

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

-- input a task list ([0,...,7]), and all constraints (in Constraint data type)
-- output its penalty (too near penalty + machine penalty)
penalty :: ([Int], Constraint, Int) -> Int
penalty ([], _, _) = 0
penalty ([x], content, mach) = getMachPenalty content!!mach!!x
penalty (x:xs, content, mach)
    -- add too near penalties of position 0 and 1, and position 7 and 0 when the first time run the function  
    | length xs == 2 = getMachPenalty content!!mach!!x + getNearPenalty content!!x!!head xs + getNearPenalty content!!last xs!!x + penalty (xs, content, mach+1)
    -- add too near penalty of current position and next position 
    | otherwise      = getMachPenalty content!!mach!!x + getNearPenalty content!!x!!head xs + penalty (xs, content, mach+1)
