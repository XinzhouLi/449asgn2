--check 8 lines, 8 columns 
--check number greater than 0
--check it is a number instead of other things

--Name
formatCheck1 :: String -> Bool
formatCheck1 [] = False
formatCheck1 x
    | not(null x) && (' ' `notElem` x) =True
    | otherwise = False

--forced partial && forbidden machine && too-near Tasks
formatCheck2 :: String -> Bool
formatCheck2 [] = False
formatCheck2 x
    | (head x == '(') && (last x == ')') && (x!!2 == ',') && (length x == 5)= True
    | otherwise = False

--too-near Penalities
formatCheck3 :: String -> Bool
formatCheck3 [] = False
formatCheck3 x
    | (head x == '(') && (last x == ')') && (x!!2 == ',') && (x!!4== ',') && (length x == 7)= True
    | otherwise = False

--machine penalties
formatCheck5 :: String -> Bool
formatCheck5 [] = False 
formatCheck5 x 
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