import FileIO
import Permutations


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
    
    print partialAsgn
    print forbiddenMa
    print toonearTasks
    print machinePens
    print toonearPens