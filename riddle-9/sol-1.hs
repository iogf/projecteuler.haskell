-- a^2 + b^2 = c^2
-- a + b + c = 1000
-- a^2 + b^2 = (1000 - a - b)^2
-- a^2 + b^2 = (1000 - a)^2 - 2(1000 - a)b + b^2
-- a^2 = (1000 - a)^2 - 2(1000b - ab)
-- a^2 = 1000^2 - 2000a + a^2 - 2000b + 2ab
-- (-1000^2 + 2000a)/(-2000+2a) = b



isSqrt :: (Integral a) => a -> Bool
isSqrt n = or $ map (== n) $ map (^2) [1 .. round $ sqrt $ fromIntegral n]

find = head [a * b * (sqrt c) | a <- [1.0, 2.0 ..], 
                         let b = (-1000^2 + 2000 * a) / (-2000 + 2 * a), 
                         let c = a^2 + b^2,
                         isSqrt $ floor c]

main = interact alpha
    where alpha input = show find ++ "\n"


