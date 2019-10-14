
import Data.Char

eftBool :: Bool -> Bool -> [Bool]
eftBool True True  = [True]
eftBool True False  = []
eftBool False True  = [False, True]
eftBool False False = [False]

splitS :: Char -> String -> [String]
splitS c s = go s []
    where go k ss
           | k == "" = reverse ss
           | otherwise =
                let w = takeWhile (/= c) k
                    r = (dropWhile (== c) . dropWhile (/= c)) k
                in go r (w:ss)

splitWords = splitS ' '

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen
            ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines = splitS '\n'

acro xs = [x | x <- xs, elem x ['A'..'Z']]

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

matrix = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

x = [1, undefined, 3]

length' :: [a] -> Integer
length' [] = 0
length' (k:xs) = 1 + length' xs

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y):(myZip xs ys)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = (f x y) : myZipWith f xs ys

capitalizeFirst :: [Char] -> [Char]
capitalizeFirst [] = []
capitalizeFirst (x:xs) = toUpper x : xs

capitalizeAll :: [Char] -> [Char]
capitalizeAll [] = []
capitalizeAll (x:xs) = toUpper x : capitalizeAll xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) = e == x || myElem e xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' e = myAny ((==) e) 

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- From prelude source
myReverse' :: [a] -> [a]
myReverse' l = rev l []
    where
        rev []     a = a
        rev (x:xs) a = rev xs (x:a)

squish :: [[a]] -> [a]
squish l = go l [] where
    go [] a = a
    go (x:xs) a = x ++ go xs a

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "no maximum for empty list"
myMaximumBy f l = go l (head l) where
    go [] a = a
    go (x:xs) a
        | f x a == GT = go xs x
        | otherwise   = go xs a

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f l = go l (head l) where
    go [] a = a
    go (x:xs) a
        | f x a == LT = go xs x
        | otherwise   = go xs a

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare