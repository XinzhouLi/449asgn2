module ConstraintsCheck(
    mainConstraintsCheck,
    checkName,
    checkForcAndForbContent,
    checkFormat2,
    checkFormat3,
    checkTooNearPen,
    checkTooNearContent,
    checkLengthRow,
    checkLengthMachinePenalty,
    checkMachinePenaltyContent,
    letterCheck,
    numCheck,
    errorPrint
)where

import FileIO
import Distribution.Simple.Utils (xargs)
import System.Posix (SystemID(machine))
import System.Exit (exitSuccess)

-- Name (phasing error)
checkName :: String -> Bool
checkName [] = False
checkName x
    | not(null x) && (' ' `notElem` x) =True
    | otherwise = False

-- forced partial && forbidden machine && too-near Tasks: check format
-- phasing error
checkFormat2 :: String -> Bool
checkFormat2 [] = True
checkFormat2 x
    | (head x == '(') && (last x == ')') && (x!!2 == ',') && (length x == 5)= True
    | otherwise = False

--forced partial && forbidden machine check
-- invalid task or pen
checkForcAndForbContent :: String -> Bool
checkForcAndForbContent [] = True
checkForcAndForbContent x
    |  numCheck (x!!1) && letterCheck (x!!3) = True
    | otherwise = False

--too-near tasks check content 
-- invalid task (too near penalty)
-- invalid task/mach(too near assign)
checkTooNearContent :: String -> Bool
checkTooNearContent [] = True
checkTooNearContent x
    | letterCheck (x!!1) && letterCheck (x!!3) = True
    | otherwise = False

--too-near Penalities check assignemnt
-- phasing error
checkFormat3 :: String -> Bool
checkFormat3 [] = True
checkFormat3 x
    | (head x == '(') && (last x == ')') && (x!!2 == ',') && (x!!4== ',') = True
    | otherwise = False

--too-near Penalities check penalty
-- invalid pen
checkTooNearPen :: String -> Bool
checkTooNearPen [] = True
checkTooNearPen x
    | penCheck (sliceList 4 ((length x)-1) x) = True
    | otherwise = False

--machine penalties
-- when use this plz (checkMachinePenaltyContent input && checkLengthMachinePenalty input) to get the full check of the machine penalties 
-- invalid pen
checkMachinePenaltyContent :: [String] -> Bool
checkMachinePenaltyContent [] = False 
checkMachinePenaltyContent [x] = machinePenNumCheck (words x)
checkMachinePenaltyContent (x:xs) =  machinePenNumCheck (words x)  && checkMachinePenaltyContent xs

machinePenNumCheck :: [String] -> Bool
machinePenNumCheck [] = False
machinePenNumCheck [x] = penCheck x 
machinePenNumCheck (x:xs) = penCheck x && machinePenNumCheck xs 

-- check each row all the num is number and it has 8 number in it
-- checkLengthMachinePenalty constraints && checkLengthRow constraints
-- phasing error
checkLengthRow :: [String] -> Bool 
checkLengthRow [] = False 
checkLengthRow [x] = checkLengthMachinePenalty (words x) 
checkLengthRow (x:xs) = checkLengthMachinePenalty (words x) && checkLengthRow xs 

checkLengthMachinePenalty :: [String] -> Bool
checkLengthMachinePenalty x 
    | length x == 8 = True
    | otherwise =  False

-- invalid machine/task
letterCheck :: Char -> Bool 
letterCheck x 
    | x `elem` ['A'..'H'] = True 
    | otherwise = False

numCheck :: Char -> Bool 
numCheck x 
    | x `elem` ['1'..'8'] = True 
    | otherwise = False

numCheck' :: Char -> Bool 
numCheck' x 
    | x `elem` ['0'..'9'] = True 
    | otherwise = False

penCheck :: String -> Bool
penCheck [] = False
penCheck [x] = numCheck' x
penCheck (x:xs) = numCheck' x && penCheck xs

--Output Errors
errorPrint :: Int -> String
errorPrint warning
    | warning == 0 = error "Name Error"
    | warning == 1 = error "partial assignment error"
    | warning == 2 = error "invalid machine/task"
    | warning == 3 = error "machine penalty error"
    | warning == 4 = error "invalid task"
    | warning == 5 = error "invalid penalty"
    | warning == 6 = error "Error while parsing input file"
    | otherwise = ""

--convert letter to integer
convLetToInt :: Char -> Int
convLetToInt 'A' = 0
convLetToInt 'B' = 1
convLetToInt 'C' = 2
convLetToInt 'D' = 3
convLetToInt 'E' = 4
convLetToInt 'F' = 5
convLetToInt 'G' = 6
convLetToInt 'H' = 7
convLetToInt x = -1

--convert number to integer
convNumToInt :: Char -> Int
convNumToInt '1' = 0
convNumToInt '2' = 1
convNumToInt '3' = 2
convNumToInt '4' = 3
convNumToInt '5' = 4
convNumToInt '6' = 5
convNumToInt '7' = 6
convNumToInt '8' = 7
convNumToInt x = -1





mainConstraintsCheck:: [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> [String] ->  IO ()
mainConstraintsCheck arg name partialAsgn forbiddenMa toonearTasks machinePens toonearPens = do
    -- check name
    if ifBoolean (map checkName name)
        then return()
    else do writeFile (last arg) "Error while parsing input file"
            exitSuccess

    -- check forced partial assignment
    if ifBoolean (map checkFormat2 partialAsgn)
        then return ()
    else do writeFile (last arg) "Error while parsing input file"
            exitSuccess
    if ifBoolean (map checkForcAndForbContent partialAsgn)
        then return ()
    else do writeFile (last arg) "invalid machine/task"
            exitSuccess
 
    -- check forbidden machine
    if ifBoolean (map checkFormat2 forbiddenMa)
        then return ()
    else do writeFile (last arg) "Error while parsing input file"
            exitSuccess
    if ifBoolean (map checkForcAndForbContent forbiddenMa)
        then return ()
    else do writeFile (last arg) "invalid machine/task"
            exitSuccess

    -- check too near task
    if ifBoolean (map checkFormat2 toonearTasks)
        then return ()
    else do writeFile (last arg) "Error while parsing input file"
            exitSuccess

    if ifBoolean (map checkTooNearContent toonearTasks)
        then return ()
    else do writeFile (last arg) "invalid machine/task"
            exitSuccess

    -- check machine penalty
    if checkLengthMachinePenalty machinePens && checkLengthRow machinePens
        then return ()
    else do writeFile (last arg) "Error while parsing input file"
            exitSuccess
    if checkMachinePenaltyContent machinePens
        then return ()
    else do writeFile (last arg) "invalid penalty"
            exitSuccess

    -- check too near penalty
    if ifBoolean (map checkFormat3 toonearPens)
        then return ()
    else do writeFile (last arg) "Error while parsing input file"
            exitSuccess
    if ifBoolean (map checkTooNearPen toonearPens) 
        then return ()
    else do writeFile (last arg) "invalid penalty"
            exitSuccess
    if ifBoolean (map checkTooNearContent toonearPens)
        then return ()
    else do writeFile (last arg) "invalid machine"
            exitSuccess
 

