
find n = foldl lcm 1 [1 .. n]


main = interact alpha
    where alpha input = show (find 20) ++ "\n"
