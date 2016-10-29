isqrt :: (Integral a) => a -> a
isqrt = floor . sqrt . fromIntegral

isPrime :: (Integral a) => a -> Bool
isPrime n = not $ any (\x -> mod n x == 0) [2 .. isqrt n]

find n = maximum $ filter (\x -> (isPrime x) && (mod n x == 0)) [1 .. isqrt n]


main = interact alpha
    where alpha input = show (find 600851475143) ++ "\n"

