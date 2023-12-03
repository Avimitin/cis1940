toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x <= 0 = []
    | otherwise = x `mod` 10 : toDigitsRev (x `div` 10)

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l =
    let db [] = []
        db [a] = [a]
        db (x : y : xs) = x : (2 * y) : db xs
     in reverse (db (reverse l))
