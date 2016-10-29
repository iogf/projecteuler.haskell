-- Summing all multiples of 3 and 5 below one bilion 


addMul n c m 
    | c >= n                              = m
    | (mod c 3 == 0) ||  (mod c 5 == 0)   = addMul n (c + 1) (m + c)
    | otherwise                           = addMul n (c + 1) m


main = interact alpha
    where alpha input = show (addMul 1000 0 0) ++ "\n"



