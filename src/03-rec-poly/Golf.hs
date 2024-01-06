{-# OPTIONS_GHC -Wall #-}

module Golf (skips) where

skips :: [a] -> [[a]]
skips inputs =
    let
        selectNthElement nth = map snd . filter (\(i, _) -> i `mod` nth == 0) . zip [1 ..]
     in
        [selectNthElement i inputs | i <- [1 .. length inputs]]
