import FileIO
import Permutations
import Algorithm
import Penalty
import ConstaintsCheck

main :: IO ()
main = do
    content <- inputFileIO
    -- print content
    -- print (findFPAindex content)
    let partialAsgn = sliceList (findFPAindex content) (findFMindex content) content
        forbiddenMa = sliceList (findFMindex content) (find2NTindex content) content
        toonearTasks = sliceList (find2NTindex content) (findMPindex content) content
        machinePens = sliceList (findMPindex content) (find2NPindex content) content
        toonearPens = sliceList (find2NPindex content) (length content) content
        constr = Constraint [0,2,-1] [[False,False,True],[True,False,False],[False,True,False]] [[False,False,True],[True,False,False],[False,False,False]] [[1,0,0],[0,2,0],[0,0,3]] [[0,2,0],[1,0,0],[0,0,0]]
        minP = MinPenListInfo [1,1,1] 10000000
        g = algo constr (permutations [0..2]) minP
    print partialAsgn
    print forbiddenMa
    print toonearTasks
    print machinePens
    print toonearPens
    print (getList g)
    print (getPen g)

    print(penalty ((getList g),constr,0))



