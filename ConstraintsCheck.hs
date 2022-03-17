module ConstraintsCheck(
    mainConstraintsCheck,
)where

import FileIO
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

-- forced partial && forbidden machine check
-- invalid task/mach
checkForcAndForbContent :: String -> Bool
checkForcAndForbContent [] = True
checkForcAndForbContent x
    |  machCheck (x!!1) && taskCheck (x!!3) = True
    | otherwise = False

-- too-near tasks and peanlty check content 
-- invalid task/mach (too near assign)
-- invalid task (too near penalty)
checkTooNearContent :: String -> Bool
checkTooNearContent [] = True
checkTooNearContent x
    | taskCheck (x!!1) && taskCheck (x!!3) = True
    | otherwise = False

-- machine penalty check format
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

-- machine penalties check peanlty
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

-- too-near Penalities check format
-- phasing error
checkFormat3 :: String -> Bool
checkFormat3 [] = True
checkFormat3 x
    | (head x == '(') && (last x == ')') && (x!!2 == ',') && (x!!4== ',') = True
    | otherwise = False

-- too-near Penalities check penalty
-- invalid pen
checkTooNearPen :: String -> Bool
checkTooNearPen [] = True
checkTooNearPen x
    | penCheck (sliceList 4 ((length x)-1) x) = True
    | otherwise = False

-- check whether input task is in the scope
taskCheck :: Char -> Bool 
taskCheck x 
    | x `elem` ['A'..'H'] = True 
    | otherwise = False

-- check whether input machine is in the scope
machCheck :: Char -> Bool 
machCheck x 
    | x `elem` ['1'..'8'] = True 
    | otherwise = False

-- check whether input number is a natural number
penCheck :: String -> Bool
penCheck [] = False
penCheck [x] = numCheck x
penCheck (x:xs) = numCheck x && penCheck xs

numCheck :: Char -> Bool 
numCheck x 
    | x `elem` ['0'..'9'] = True 
    | otherwise = False

ifBoolean :: [Bool] -> Bool
ifBoolean [] = True
ifBoolean xs = and xs

-- input all contents from the input file (Name, Forced Partial, Forbidden, Too-Near Assign, Machine Penalty, and Too-Near Penalty)
-- output nothing if there is no invalid input, otherwise, write the invalid message to the file
mainConstraintsCheck:: [String] -> [String] -> [String] -> [String] -> [String] -> [String] ->  IO ()
mainConstraintsCheck name partialAsgn forbiddenMa toonearTasks machinePens toonearPens = do
    -- check name
    if ifBoolean (map checkName name)
        then return()
    else do outputFileIO "Error while parsing input file"
            exitSuccess

    -- check forced partial assignment
    if ifBoolean (map checkFormat2 partialAsgn)
        then return ()
    else do outputFileIO "Error while parsing input file"
            exitSuccess
    if ifBoolean (map checkForcAndForbContent partialAsgn)
        then return ()
    else do outputFileIO "invalid machine/task"
            exitSuccess
 
    -- check forbidden machine
    if ifBoolean (map checkFormat2 forbiddenMa)
        then return ()
    else do outputFileIO "Error while parsing input file"
            exitSuccess
    if ifBoolean (map checkForcAndForbContent forbiddenMa)
        then return ()
    else do outputFileIO "invalid machine/task"
            exitSuccess

    -- check too near task
    if ifBoolean (map checkFormat2 toonearTasks)
        then return ()
    else do outputFileIO "Error while parsing input file"
            exitSuccess

    if ifBoolean (map checkTooNearContent toonearTasks)
        then return ()
    else do outputFileIO "invalid machine/task"
            exitSuccess

    -- check machine penalty
    if checkMachinePenaltyContent machinePens
        then return ()
    else do outputFileIO "invalid penalty"
            exitSuccess
    if checkLengthMachinePenalty machinePens && checkLengthRow machinePens
        then return ()
    else do outputFileIO "machine penalty error"
            exitSuccess


    -- check too near penalty
    if ifBoolean (map checkFormat3 toonearPens)
        then return ()
    else do outputFileIO "Error while parsing input file"
            exitSuccess
    if ifBoolean (map checkTooNearPen toonearPens) 
        then return ()
    else do outputFileIO "invalid penalty"
            exitSuccess
    if ifBoolean (map checkTooNearContent toonearPens)
        then return ()
    else do outputFileIO "invalid task"
            exitSuccess
 
