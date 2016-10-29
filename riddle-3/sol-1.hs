sieve n = sieve' 2 [2..n] [] 
    where
    sieve' q xs ms
        | null xs    = ms
        | otherwise  = sieve' (head ns) ns (q:ms) 
        where 
            ns = [x  | x <- xs, 
                 let muls = takeWhile (<= n) (map (*q) [1..]),
                 not (elem x muls)]


isFactor = \x y -> mod x y == 0
divisors n = filter (isFactor n) (sieve n)

main = interact alpha
    where
        result = foldl max 0 (divisors 600851475143)
        alpha input = show result ++ "\n"    

-- fail

