--check 8 lines, 8 columns 
--check number greater than 0
--check it is a number instead of other things
--Error while parsing input file
formatCheck2 :: String -> Bool
formatCheck2 [] = False
formatCheck2 x
    | (head x == '(') && (last x == ')') && (x!!2 == ',') && (length x == 5)= True
    | otherwise = False

formatCheck3 :: String -> Bool
formatCheck3 [] = False
formatCheck3 x
    | (head x == '(') && (last x == ')') && (x!!2 == ',') && (x!!4== ',') && (length x == 7)= True
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