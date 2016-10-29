fib n = fib' n 1 1
    where 
    fib' n a b 
        | n > 2     = fib' (n - 1) b (a + b)
        | otherwise = b


x = sum (takeWhile (< 4000000) [term | i <- [1 ..], let term = fib i, mod term 2 == 0])
