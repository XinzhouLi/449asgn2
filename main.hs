import FileIO
import Algorithm
import ConstraintsCheck
import ConstraintConstr
import System.Exit (exitSuccess)
import System.Directory.Internal.Prelude (getArgs)

main :: IO ()
main = do
    args <- getArgs
    content <- inputFileIO
    -- print content
    -- print (findFPAindex content)
    if ("Name:" `elem` content) && ("forced partial assignment:" `elem` content) && ("forbidden machine:" `elem` content) && ("too-near tasks:" `elem` content) && ("machine penalties:" `elem` content) &&("too-near penalities" `elem` content)
        then return()
    else do outputFileIO "Error while parsing input file"
            exitSuccess
    let name = sliceList (findNameindex content) (findFPAindex content) content
        partialAsgn = sliceList (findFPAindex content) (findFMindex content) content
        forbiddenMa = sliceList (findFMindex content) (find2NTindex content) content
        toonearTasks = sliceList (find2NTindex content) (findMPindex content) content
        machinePens = sliceList (findMPindex content) (find2NPindex content) content
        toonearPens = sliceList (find2NPindex content) (length content) content
    -- print partialAsgn
    -- print forbiddenMa
    -- print toonearTasks
    -- print machinePens
    -- print toonearPens

    let
        --Construct constraints
        constraints = constraintConstructor partialAsgn forbiddenMa toonearTasks machinePens toonearPens
        result = findMin constraints (MinPenListInfo [0,0,0,0,0,0,0,0] (maxBound :: Int))

    --Error check
    do mainConstraintsCheck name partialAsgn forbiddenMa toonearTasks machinePens toonearPens

    if getForcedPartial constraints /= [0,0,0,0,0,0,0,0]
        then return()
    else do outputFileIO "partial assignment error"
            exitSuccess

    --Calculate the reuslt
    if (getList result) /= [0,0,0,0,0,0,0,0]
        then return()
    else do outputFileIO "No valid solution possible!"
            exitSuccess

    outputFileIO (resultOutput (getList result) (getPen result))
    print (resultOutput (getList result) (getPen result))
