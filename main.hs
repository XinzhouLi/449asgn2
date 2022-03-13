import FileIO
import Permutations
import Algorithm
import Penalty
import ConstaintsCheck
import System.Exit (exitSuccess)
import System.Directory.Internal.Prelude (getArgs)

main :: IO ()
main = do
    args <- getArgs
    content <- inputFileIO
    -- print content
    -- print (findFPAindex content)
    let name = sliceList (findNameindex content) (findFPAindex content) content
        partialAsgn = sliceList (findFPAindex content) (findFMindex content) content
        forbiddenMa = sliceList (findFMindex content) (find2NTindex content) content
        toonearTasks = sliceList (find2NTindex content) (findMPindex content) content
        machinePens = sliceList (findMPindex content) (find2NPindex content) content
        toonearPens = sliceList (find2NPindex content) (length content) content
        constr = Constraint [0,2,-1] [[False,False,True],[True,False,False],[False,True,False]] [[False,False,True],[True,False,False],[False,False,False]] [[0,0,0],[0,2,0],[0,0,3]] [[0,2,0],[1,0,0],[0,0,0]]
        minP = MinPenListInfo [0,1,2] (penalty ([0,1,2], constr, 0))
        g = algo constr (permutations [0..2]) minP
    print partialAsgn
    print forbiddenMa
    print toonearTasks
    print machinePens
    print toonearPens
    -- print (getList g)
    -- print (getPen g)

    -- print(penalty ((getList g), constr, 0))


    --Error check
    do mainConstraintsCheck args name partialAsgn forbiddenMa toonearTasks machinePens toonearPens
