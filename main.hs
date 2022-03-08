import Penalty

main = do
    let content = Constraint [0,1,-1] [[False,False,True],[True,False,False],[False,True,False]] [[False,False,True],[True,False,False],[False,False,False]] [[1,0,0],[0,0,1],[0,1,0]] [[0,0,1],[1,0,0],[0,1,0]]
    print(penalty ([0,2,1], content, 0))
    -- A, C, B