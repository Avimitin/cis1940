import Data.List

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldl' (\acc x -> (x - 2) * acc) 1 . filter even
