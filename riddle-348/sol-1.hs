import Data.List

cdigit n = [(u, v) | u <- [0 .. 9], v <- [0 .. 9],  (u^2 + v^3) `mod` 10 == n]


-- sseq n = [(u + e, v + s) | u <- takeWhile ((< n) . (^2)) [0, 10 .. n], 
--                           v <- takeWhile ((< n) . (^3)) [0, 10 .. u], 
--                           (e, s) <- cdigit (mod n 10)] 

-- isexpr n = [(m, u) | (m, u) <- sseq n, 
--                     m^2 + u^3 == n]

isexpr n = length [(m, u) | m <- (takeWhile ((< n) . (^2)) [1 .. n]), 
                     u <- (takeWhile ((< n) . (^3)) [1 .. m]), 
                     m^2 + u^3 == n] == 4



-- buildPalindrom :: Integer -> [Integer]
-- buildPalindrom n = buildPalindrom' (div n 10) (mod n 10) 0
--    where
--    buildPalindrom' a b c
--        | a == 0    = [n * 10 ^ (c + 1) + b, (div n 10) * 10 ^ (c + 1) + b]
--        | otherwise = buildPalindrom' (div a 10) (b * 10 + (mod a 10)) (c + 1)

revn :: Integer -> Integer
revn n = revn' (div n 10) (mod n 10)
    where
    revn' a b
        | a == 0    = b
        | otherwise = revn' (div a 10) $ b * 10 + (mod a 10)


--seqp = [[n, u, v] | u <- [1 ..], v <- [1 ..u], let n = u^2 + v^3, show n == (reverse (show n))]

ispal n = show n == (reverse (show n))

seqp :: [Integer]
seqp = [n | u <- [1 ..], v <- (takeWhile (not . ispal . ((u^2 +) . (^3))) [2 .. u]), let n = u^2 + v^3]





