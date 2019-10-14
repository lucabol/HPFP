import Data.List (intersperse)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n-1) f b)

-- applyTimes 5 (+1) 5
--- (+1) (applyTimes 4 (+1) 5)
---- (+1) ((+1) (applyTimes 3 (+1) 5)))
----- (+1)((+1) ((+1) (applyTimes 2 (+1) 5))))
------ (+1)((+1) ((+1) ((+1) (applyTimes 1 (+1) 5)))
------- (+1)((+1) ((+1) ((+1) ((+1) (applyTimes 0 (+1) 5)))
-------- (+1)((+1) ((+1) ((+1) ((+1) (5)))))

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times (+1) n

fib' :: (Integral a) => [a] -> [a]
fib' (x:y:xs) = ((x + y):x:y:xs)
fib' [x]      = [x]
fib' []       = []

fibRec :: (Integral a, Integral b, Eq b) => [a] -> b -> [a]
fibRec xs 0       = reverse xs
fibRec (x:y:xs) n = fibRec (x+y:x:y:xs) (n - 1)

fibSeries :: (Integral a, Eq a) => a -> [a]
fibSeries 0 = []
fibSeries 1 = [1]
fibSeries 2 = [1,0]
fibSeries n = fibRec [1, 0] (n - 2)

fib'' = head . reverse . fibSeries

fib :: Integral a => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibSeries' :: Integral a => a -> [a]
fibSeries' 0 = [0]
fibSeries' 1 = [0, 1]
fibSeries' n = fibSeries (n -1) ++ [fib (n-1)]

dividedBy :: Integral a => a -> a -> (a,a)
dividedBy x y = go x y 0
        where go n d count
                | n < d     = (count, n)
                | otherwise = go (n - d) d (count + 1)

mySum :: Integral a => a -> a
mySum 0 = 0
mySum n = n + mySum (n - 1)

myProd :: Integral a => a -> a -> a
myProd _ 0 = 0
myProd 0 _ = 0
myProd x y = go y 0
    where go count sum
            | count == 0 = sum
            | otherwise  = go (count - 1) (sum + x) 

data DividedResult =
    Result Integer
    | DividedByZero
    deriving Show

divBy :: Integral a => a -> a -> DividedResult
divBy _ 0 = DividedByZero
divBy x y = val
        where go n d count
                | n < d     = count
                | otherwise = go (n - d) d (count + 1)
              absVal = go (abs x) (abs y) 0
              val
                | signum x == signum y = Result absVal
                | otherwise            = Result (negate absVal)

mc91 :: Integral a => a -> a
mc91 n
    | n > 100  = n - 10
    | n <= 100 = mc91 (mc91 (n + 11))

digitToWord :: Int -> String
digitToWord = concat . intersperse "-" . map wordNumber . digits

digits :: Int -> [Int]
digits n = go n []
    where
        go k xs
            | rem == 0   = (digit:xs)
            | otherwise  = go rem (digit:xs) 
                where
                    digit = k `mod` 10
                    rem   = k `div` 10
            

        

digitWord = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

wordNumber :: Int -> String
wordNumber = (digitWord !!)