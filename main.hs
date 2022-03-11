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


    --Error check
    --check if the name correct
    if ifBoolean (map checkName(deletEmp name))
        then return()
    else do writeFile (last args) "Name Error"
            exitSuccess

    --check if the forced partial assignment are good formate
    if ifBoolean (map checkFormat(deletEmp partialAsgn))
        then return()
    else do writeFile (last args) "invalid machine/task"
            exitSuccess
 
    -- to check if the forbidden machines are are good formate
    if ifBoolean (map checkFormat(deletEmp forbiddenMa))
        then return()
    else do writeFile (last args) "invalid machine/task"
            exitSuccess

    -- to check if the too-near tasks are are good formate
    if ifBoolean (map checkFormat(deletEmp toonearTasks))
        then return()
    else do writeFile (last args) "invalid machine/task"
            exitSuccess

    -- to check if the machine penalties are a pair of three elements
    -- if ifBoolean (map check3Pair(deletEmp machinePens)) 
    -- then return()
    -- else do writeFile (last args) "invalidd machine/task"
    --         exitSuccess

    -- to check if the too-near penalties are are good formate
    if ifBoolean (map checkTooNearP(deletEmp toonearPens)) 
        then return()
    else do writeFile (last args) "invalid penalty"
            exitSuccess

    -- The text has been successfully passed
    writeFile (last args) "file parsed without problems"
    exitSuccess




