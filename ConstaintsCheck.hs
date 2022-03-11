module ConstainsCheck(
    checkName,
    checkFormat,
    checkTooNearP,
    checkMachinePenalty,
    letterCheck,
    numCheck,
    errorPrint
)where
    
--Name
checkName :: String -> Bool
checkName [] = False
checkName x
    | not(null x) && (' ' `notElem` x) =True
    | otherwise = False

--forced partial && forbidden machine && too-near Tasks
checkFormat :: String -> Bool
checkFormat [] = False
checkFormat x
    | (head x == '(') && (last x == ')') && (x!!2 == ',') && (length x == 5)= True
    | otherwise = False

--too-near Penalities
checkTooNearP :: String -> Bool
checkTooNearP [] = False
checkTooNearP x
    | (head x == '(') && (last x == ')') && (x!!2 == ',') && (x!!4== ',') && (length x == 7)= True
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

numCheck :: Int -> Bool 
numCheck x 
    | x `elem` [1..8] = True 
    | otherwise = False

--Output Errors
errorPrint :: Int -> String
errorPrint warning
    | warning == 0 = "Name Error"
    | warning == 1 = "partial assignment error"
    | warning == 2 = "invalid machine/task"
    | warning == 3 = "machine penalty error"
    | warning == 4 = "invalid task"
    | warning == 5 = "invalid penalty"
    | warning == 6 = "Error while parsing input file"
    | otherwise = ""
