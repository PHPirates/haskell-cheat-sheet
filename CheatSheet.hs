-- this doesn't compile, I know --

-- search built-in stuff
-- https://www.haskell.org/hoogle/

module CheatSheet where

import Data.String.Strip

--main :: IO ()
--main = interact strip

-- learnyouahaskell.com

--main = putStrLn "Hi test 2"

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

-- pattern match definition
(pattern, match) = (4, 2)

-- let .. in
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea

-- let is an expression, just like if .. else
fourtytwo = 4 * (let a = 9 in a + 1) + 2

-- case expression
head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head for empty lists!"
                       (x:_) -> x

-- currying
-- max :: (Ord a) => a -> a -> a
-- is equivalent to
-- max :: (Ord a) => a -> (a -> a)
--  if we call a function with too few parameters, we get back a partially applied function

-- because \ looks like a lambda
lambda = \x y -> length x > y

-- folds: use binary function to update an accumulator, starting value, input
-- use for traversing lists to return something
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- scanl is similar but accumulates a list
increasingList = scanl (+) 0 [3,5,2,1]

-- function application, $ is right-associative
sum (map sqrt [1..130]) == sum $ map sqrt [1..130]

-- function composition, . works like \circ
allNegative = map (negate . abs) [-1, 2, -3, 4]

-- readability, preferred style
oddSquareSum :: Integer
oddSquareSum =
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in  sum belowLimit


----------------------------- modules -------------------------------------

-- import modules in GHCI
:m + Data.List Data.Map

-- importing functions only
import Data.List (nub, sort)

-- catching errors with Maybe, Just, Nothing
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

-- begin filecontents Geometry.hs --

-- it exports the following functions:
module Geometry
( sphereVolume
, sphere Area
) where

sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius ^ 2)


-- end filecontents Geometry.hs --

-- use a module from the same folder
import Geometry

-- submodules, file Sphere.hs in folder Geometry
module Geometry.Sphere
( volume
, area
) where


-- qualified imports make you use it like Geometry.Sphere.area instead of area to resolve duplicates
import qualified Geometry.Sphere
-- or like Sphere.area
import qualified Geometry.Sphere as Sphere



---------------------- custom types and typeclasses -----------------------------

-- using value constructors
data Shape = Circle Float Float Float | Rectangle Float Float Float Float

-- value constructions are functions, :t Circle yields (Circle is not a type, Shape is)
Circle :: Float -> Float -> Float -> Shape

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- make objects printable by adding the show function
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- export all value constructors for data types:
module Shapes
( Point(..)
) where

-- data types using record syntax
data Person = Person { firstName :: String
                     , phoneNumber :: String
                     } deriving (Show)

-- now you can do
let guy = Person {firstName="Buddy", phoneNumber="526-2928"}
firstName guy


-- type parameters

-- type constructor = value constructor
data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)


-- the Int type is an instance of the Eq typeclass
-- Eq is a typeclass with an interface: the functions == and /=

-- implementing Eq functions is automatically done element wise
data Person = Person { firstName :: String, lastName :: String } deriving (Eq)
-- in the case of Ord, value constructors on the right are greater


-- enums
data Day = Monday | Tuesday | Friday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)
workDays = [Monday .. Friday]

-- type synonyms
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

-- Map, Int, String are concrete types, Maybe is a type constructor

-- when a Maybe is not good enough, use Either a b = Left a | Right b to provide more info, for example an error message
Left True :: Either Bool b

-- Cons is equivalent to : (: is actually a constructor, taking listHead and listTail)
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- fixity: how tightly the operator binds and whether left- or right-associative
-- higher fixity is stronger binding
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)


-- defining a typeclass
-- a is the type variable
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)

-- defining type instances
data TrafficLight = Red | Yellow | Green
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"
-- can also use type constructors like Maybe
instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False

-- Functor typeclass
class Functor f where
    fmap :: (a -> b) -> f a -> f b
-- map is a kind of fmap, defined on lists
Just 400 == fmap (*2) (Just 200)
Nothing == fmap (*2) Nothing

