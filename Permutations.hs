module Permutations(
    prefix,
    suffix,
    interleave,
    permutations
)where

prefix [] = [[]]
prefix (x : xs) = [] : [x : y | y <- prefix xs]

suffix [] = [[]]
suffix (x :xs) = (x : xs) : suffix xs

interleave x xs = [ys ++ [x]++ zs | (ys, zs) <- zip (prefix xs) (suffix xs)]

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x : xs) = [zs | ys <- permutations xs, zs <- interleave x ys]