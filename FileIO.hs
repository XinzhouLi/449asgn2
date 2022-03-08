{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use elemIndices" #-}

module FileIO(
    inputFileIO,
    outputFileIO,
    removeEmpty,
    sliceList,
    find2NPindex,
    find2NTindex,
    findFMindex,
    findFPAindex,
    findMPindex
)where

import Permutations
import System.Environment
import System.IO
import Data.List (findIndex, findIndices)
import Data.Text (stripEnd)
import qualified Data.Text as T
rstrip::String->String 
rstrip = T.unpack . T.stripEnd . T.pack





inputFileIO :: IO[String]
inputFileIO = do
    args <- getArgs
    -- normal file IO - read file and store in contents
    contents <- readFile (head args)
    return (drop 2 (removeEmpty(map rstrip(lines contents))))



outputFileIO :: String -> IO ()
outputFileIO message = do
    args <- getArgs
    writeFile (last args) message

removeEmpty :: [String] -> [String]
removeEmpty [] = []
removeEmpty (x:xs)
    | x == "" = removeEmpty xs
    | otherwise = x:removeEmpty xs

sliceList :: Int -> Int -> [a] -> [a]
sliceList front end list = drop (front + 1) (take (end) list)

findFPAindex :: [[Char]] -> Int
findFPAindex list = head (findIndices (=="forced partial assignment:") list)

findFMindex :: [[Char]] -> Int
findFMindex list = head (findIndices (=="forbidden machine:") list)

find2NTindex :: [[Char]] -> Int
find2NTindex list = head (findIndices (=="too-near tasks:") list)

findMPindex :: [[Char]] -> Int
findMPindex list = head (findIndices (=="machine penalties:") list)

find2NPindex :: [[Char]] -> Int
find2NPindex list = head (findIndices (=="too-near penalities") list)