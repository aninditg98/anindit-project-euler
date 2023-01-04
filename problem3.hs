import Debug.Trace
divideOut :: Int -> Int -> Int 
divideOut n f =
    if (n `mod` f) /= 0
        then n
    else
        divideOut (n `quot` f) f


factorize n f
    | n == 1 = []
    | f * f > n = [n]
    | otherwise = factor ++ factorize divideOutResult (f + 1)
    where 
        divideOutResult = divideOut n f
        factor = [f | divideOutResult /= n]

problem3 n = maximum (factorize n 2)
