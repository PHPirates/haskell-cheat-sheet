module Main where

import Data.String.Strip

--main :: IO ()
--main = interact strip

-- learnyouahaskell.com

main = putStrLn "Hi test 2"

doubleMe x = x * x

doubleSmallNumber x = (if x > 100 then x else doubleMe x) + 1

----------- lists -------------------------

someList = [1, 2, 3]

-- Append
someList' = someList' ++ [4, 5, 6]

-- Prepend
someList'' = 0:someList'

someString = "hi " ++ "haskell"

stringsAndLists = ['a', 'b'] == "a" ++ "b"

syntacticSugar = [1,2,3] == 1:2:3:[]

selectFromList = "Fromthis" !! 3

compareLexicographically = [3, 2, 1] > [2, 2, 1]

-- Other list functions: head, tail, last, init, length, null, reverse, take, drop, maximum, minimum, sum, product, elem

evenNrs = [-4,-2..4]

lazyList = [13, 26..24*13] == take 24 [13,26..]

-- cycle, repeat, replicate

listComprehension = [2*x | x <- [1..10], 2*x >= 12]

flatListProduct = [x*y | x <- [1,2,3], y <- [3,2,1]]

-------------------- typeclasses ----------------------------

-- function signature:
--removeNonUppercase :: [Char] -> [Char]
removeNonUppercase :: String -> String -- not required.
-- or use String -> String -> String, or check with myFun :t
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- :t (==)

-- Typeclasses: Eq, Ord, Show, Read, Enum, Bounded, Num

isFive = read "5" :: Int
isAlsoFive = read "7" - 2

comparisons = [LT .. GT]

------------------------- function syntax --------------------------

factorial :: (Integral a) => a -> a
-- pattern matching:
factorial 0 = 1
factorial n = n * factorial (n - 1)

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

-- as patterns:
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- guards and local variables

describeNumber x
  | y <= 0 = "Very small."
  | y `elem` [1..10] = "Small."
  | otherwise = "Large."
  where z = x^2
        y = z + 1

-- infix definition
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT
