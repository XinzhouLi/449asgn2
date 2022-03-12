module ConstaintsCheck(
    checkName,
    checkFormat,
    -- checkTooNearP,
    checkMachinePenalty,
    letterCheck,
    numCheck,
    errorPrint
)where

import FileIO

--Name
checkName :: String -> Bool
checkName [] = False
checkName x
    | not(null x) && (' ' `notElem` x) =True
    | otherwise = False

--forced partial && forbidden machine && too-near Tasks: check format
checkFormat :: String -> Bool
checkFormat [] = False
checkFormat x
    | (head x == '(') && (last x == ')') && (x!!2 == ',') && (length x == 5)= True
    | otherwise = False

--forced partial && forbidden machine check
checkForcAndForb :: String -> Bool
checkForcAndForb [] = False
checkForcAndForb x
    | checkFormat x && numCheck (x!!1) && letterCheck (x!!3) = True
    | otherwise = False

--too-near tasks check
checkTooNear :: String -> Bool
checkTooNear [] = False
checkTooNear x
    | checkFormat x && letterCheck (x!!1) && letterCheck (x!!3) = True
    | otherwise = False

--too-near Penalities check assignemnt
checkTooNearAssign :: String -> Bool
checkTooNearAssign [] = False
checkTooNearAssign x
    | (head x == '(') && (last x == ')') && (x!!2 == ',') && (x!!4== ',') = True
    | otherwise = False

--too-near Penalities check penalty
checkTooNearPen :: String -> Bool
checkTooNearPen [] = False
checkTooNearPen x
    | penCheck (sliceList 4 ((length x)-1) x) = True
    | otherwise = False

--machine penalties
checkMachinePenalty :: String -> Bool
checkMachinePenalty [] = False 
checkMachinePenalty x 
    | (length x == 15) && (x!!1 == ' '&&x!!3 == ' '&& x!!5 == ' '&& x!!7 == ' ' && x!!9 == ' '&& x!!11 == ' '&& x!!13 == ' ') = True 
    | otherwise = False

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