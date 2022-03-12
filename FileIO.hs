{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use elemIndices" #-}

module FileIO(
    inputFileIO,
    outputFileIO,
    deletEmp,
    ifBoolean,
    sliceList,
    sliceList',
    findNameindex,
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
    return (deletEmp(map rstrip(lines contents)))



outputFileIO :: String -> IO ()
outputFileIO message = do
    args <- getArgs
    writeFile (last args) message

deletEmp :: [String] -> [String]
deletEmp [] = []
deletEmp (x:xs)
    | x == "" = deletEmp xs
    | otherwise = x:deletEmp xs

ifBoolean :: [Bool] -> Bool
ifBoolean [] = True
ifBoolean xs = and xs

sliceList :: Int -> Int -> [a] -> [a]
sliceList front end list = drop (front + 1) (take (end) list)

sliceList' :: Int -> Int -> [a] -> [a]
sliceList' front end list = drop (front) (take (end) list)

findNameindex :: [[Char]] -> Int 
findNameindex list = head (findIndices (=="Name:") list)

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