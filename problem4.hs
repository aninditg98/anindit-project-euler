listifyNumber n
    | n == 0 = []
    | otherwise =  listifyNumber (n `quot` 10) ++ [(n `mod` 10)]

removeFirstAndLast l = drop 1 (take (length l - 1) l)

isListPalindrome l
    | null l = True
    | length l == 1 = True
    | head l == last l = isListPalindrome (removeFirstAndLast l)
    | otherwise = False

isNumberPalindrome n = isListPalindrome (listifyNumber n)

problem4 = maximum [ x*y | x <- [100..999], y <- [100..999], isNumberPalindrome (x*y)]