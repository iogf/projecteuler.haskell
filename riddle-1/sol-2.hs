addMul m = sum [n | n <- [1..m], n `mod` 5 == 0 || n `mod` 3 == 0]


main = interact alpha
    where alpha input = show (addMul 1000) ++ "\n"


