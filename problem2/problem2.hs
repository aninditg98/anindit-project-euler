import Debug.Trace
problem2 a b =
    if b > 4000000
        then to_add
    else
        to_add + problem2 b (a + b)
    where to_add = if even b then b else 0