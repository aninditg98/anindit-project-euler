import Data.Set

divideOut :: Int -> Int -> Int 
divideOut n f =
    if (n `mod` f) /= 0
        then n
    else
        divideOut (n `quot` f) f

_factorize n f
    | n == 1 = []
    | f * f > n = [n]
    | otherwise = factor ++ _factorize divideOutResult (f + 1)
    where 
        divideOutResult = divideOut n f
        factor = [f | divideOutResult /= n]

factorize n = _factorize n 2

_factorizeFactorial n 
    | n == 1 = []
    | otherwise = factorize n ++ _factorizeFactorial (n-1)

factorizeFactorial n = toList (fromList (_factorizeFactorial n))

multiplyList l
    | Prelude.null l = 1
    | otherwise = head l * multiplyList (tail l)

problem5 = multiplyList (factorizeFactorial 20)
