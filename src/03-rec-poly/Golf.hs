{-# OPTIONS_GHC -Wall #-}

module Golf (skips, localMaxima) where

skips :: [a] -> [[a]]
skips inputs =
    let
        selectNthElement nth = map snd . filter (\(i, _) -> i `mod` nth == 0) . zip [1 ..]
     in
        [selectNthElement i inputs | i <- [1 .. length inputs]]

localMaxima :: [Integer] -> [Integer]
localMaxima (x : y : z : xs) = [y | y > x && y > z] ++ localMaxima (y : z : xs)
localMaxima _ = []
