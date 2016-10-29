{- 
By replacing each of the letters in the word CARE with 1, 2, 9, and 6 respectively, we form a square number: 1296 = 362. What is remarkable is that, by using the same digital substitutions, the anagram, RACE, also forms a square number: 9216 = 962. We shall call CARE (and RACE) a square anagram word pair and specify further that leading zeroes are not permitted, neither may a different letter have the same digital value as another letter.

Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common English words, find all the square anagram word pairs (a palindromic word is NOT considered to be an anagram of itself).

What is the largest square number formed by any member of such a pair?

NOTE: All anagrams formed must be contained in the given text file.
http://projecteuler.net/problem=98

Answer: 18769

by tau
-}

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

split :: Char -> String -> [String]
split delim [] = []
split delim xs = split' xs
    where 
        split' [] = []
        split' xs = (take sp xs):(split' ys)
            where 
                len = length xs
                si  = elemIndex delim xs
                sp  = fromMaybe len si
                ns  = take sp xs
                ys  = drop (sp + 1) xs


loadData :: IO [String]
loadData = do x <- readFile "words.txt"
              return $ split ',' (filter (/= '"') x)
              

findAnagram :: String -> [String] -> [String]
findAnagram word xs = filter ((== (sort word)) . sort) xs

-- rep :: (Eq a) => a -> [a] -> [a]
rep ns xs = filter (\x -> not (elem x ns)) xs

polarize [] = []
polarize xs = box:(polarize (rep box xs)) 
    where x = head xs
          box = findAnagram x xs

alpha max = takeWhile ((< max) . length) $ map show $ map (^2) [1 .. ]

isCompatible xs ys = length xs == (length ys) && (Set.size (Set.fromList xs) == (Set.size (Set.fromList ys)))

mirror xs con = map (\x -> Map.findWithDefault '0' x con) xs


-- m = Map.fromList [('a', '1'), ('b', '2')]

isSquareAnagramPair xs ys 
    | not $ isCompatible x y    = False
    | oc >= 2                   = True
    where x     = head xs
          y     = head ys
          con   = Map.fromList (zip x y)
          oc    = sum [1 | i <- xs, elem (mirror i con) ys]
isSquareAnagramPair _ _ = False

groupSquarePair xs ys = [(x, y) | x <- xs, y <- ys, 
                                  let a = head xs,
                                  let b = head ys,
                                  let con = Map.fromList (zip a b), 
                                  mirror x con == y]

polarizeAnagrams = do db <- loadData
                      let u = filter ((> 1) . length) $ polarize db
                      let v = filter ((> 1) . length) $ polarize (alpha 8)
                      return $ [groupSquarePair x y | x <- u, y <-v, isSquareAnagramPair x y]

findLargestSquareNumber = do web <- polarizeAnagrams
                             let g = maximum $ map (\x -> read x :: Integer) [snd j | i <- web, j <- i]    
                             return g

main = do g <- findLargestSquareNumber
          print g




