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
append' = undefined -- TODO

































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
headOr = undefined

-- guards
javaCompare :: Int -> Int -> Int
javaCompare = undefined

-- let/in and where

-- toTitleCase :: String -> String
-- toTitleCase s =
--   allWords = words s
--   capitalised = map capitalise allWords
--   unwords capitalised

capitalise (c:cs) = toUpper c : cs

-- pattern matching

-- on list
safeHead :: [a] -> Maybe a
safeHead = undefined

-- on tuple
swap :: (a, b) -> (b, a)
swap = undefined

-- on data
valueOr :: a -> Maybe a -> a
valueOr = undefined

-- recursion

count :: [a] -> Int
count = undefined

listBetween :: Int -> Int -> [Int]
listBetween from to = go to []
 where go n l | n == from = n : l
              | otherwise = go (n - 1) (n : l)























-- Changing the world with `IO`

interactiveGrettings :: IO String
interactiveGrettings = do
  putStr "What's your name?: "
  name <- getLine
  putStrLn $ grettings name
  return name





