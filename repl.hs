import Data.Char (toUpper)

-- Basic types

boolean :: Bool
boolean = True

int :: Int
int = 1234

integer :: Integer
integer = 9999999999999999999999999999999999999999999999999999999999999

float :: Float
float = 1234.56

char :: Char
char = 'a'

string :: String
string = "lalala"

-- List

intList :: [Int]
intList = [1, 2, 3]

boolList :: [Bool]
boolList = [True, False]

charList :: [Char] -- aka String
charList = "lelele"

-- Tuple

tuple :: (Int, Bool)
tuple = (1, True)

tuple3 :: (Int, Bool, Char)
tuple3 = (2, True, 'a')

-- Function

double :: Int -> Int
double = \n -> n + n

append' :: String -> String -> String
append' s s2 = s ++ s2

































-- Type Inference. Scripting with Haskell
-- Look mum! Like python.

grettingsPrefix = "Hello, "

grettings name = grettingsPrefix ++ name

callGrettings = grettings "John"






























-- Polymorphic functions

heads :: [[a]] -> [a]
heads lists = map head lists

-- Defining functions

-- conditional
headOr :: a -> [a] -> a
headOr a l = if null l then a else head l

-- guards
javaCompare :: Int -> Int -> Int
javaCompare n1 n2 | n1 < n2 = -1
                  | n1 == n2 = 0
                  | otherwise = 1

-- let/in and where

toTitleCase :: String -> String
toTitleCase s =
  let allWords = words s
      capitalised = map capitalise allWords
  in  unwords capitalised
 where
  capitalise (c:cs) = toUpper c : cs

-- pattern matching

-- on list
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : _) = Just a

-- on tuple
swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

-- on data
valueOr :: a -> Maybe a -> a
valueOr a Nothing = a
valueOr _ (Just a) = a

-- recursion

count :: [a] -> Int
count [] = 0
count (_:as) = 1 + count as

listBetween :: Int -> Int -> [Int]
listBetween from to = go to []
 where go n l | n == from = n : l
              | otherwise = go (n - 1) (n : l)























-- Changing the world with `IO`

interactiveGrettings :: IO ()
interactiveGrettings = do
  putStr "What's your name?: "
  name <- getLine
  putStrLn $ grettings name





