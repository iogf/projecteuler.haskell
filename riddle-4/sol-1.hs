{-
magoo :: (Num a) => (a->a) -> (a->a) -> a -> [a]
magoo f g n = magoo' n []
    where
    magoo' 0 xs = xs
    magoo' n xs = magoo' (f n) (g n:xs)

reverseNumber :: Integer -> Integer
reverseNumber n = reverseNumber' (div n 10) (mod n 10)
    where
    reverseNumber' a b
        | a == 0    = b
        | otherwise = reverseNumber' (div a 10) $ b * 10 + (mod a 10)
-}

buildPalindrom :: Integer -> [Integer]
buildPalindrom n = buildPalindrom' (div n 10) (mod n 10) 0
    where
    buildPalindrom' a b c
        | a == 0    = [n * 10 ^ (c + 1) + b, (div n 10) * 10 ^ (c + 1) + b]
        | otherwise = buildPalindrom' (div a 10) (b * 10 + (mod a 10)) (c + 1)

isProduct :: Integer -> [Integer] -> [Integer] -> Bool
isProduct n [a, b] [c, d] = any (== True) [ rn == 0  && (dn >= c) && (dn <= d)  | i <- [a .. b], let dn = div n i, let rn = mod n i]

findLargestPalindrom :: [Integer] -> [Integer] -> Integer
findLargestPalindrom m v = maximum $ filter (\x -> isProduct x m v) $ takeWhile (< (maximum m * maximum v)) [x |  x <- concat $ map buildPalindrom [1 .. ]]


main = interact alpha
    where alpha input = show (findLargestPalindrom [100, 1000] [100, 1000]) ++ "\n"

