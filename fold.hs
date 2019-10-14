import Data.Time
import Data.List

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x b -> f x || b) False xs

-- if the list is empty, the result is the initial value z; else
-- apply f to the first element and the result of folding the rest
foldr' f z []     = z 
foldr' f z (x:xs) = f x (foldr' f z xs) 

-- if the list is empty, the result is the initial value; else
-- we recurse immediately, making the new initial value the result
-- of combining the old initial value with the first element.
foldl'' f z []     = z                  
foldl'' f z (x:xs) = foldl'' f (f z x) xs

data DatabaseItem = DbString String
    | DbNumber Integer
    | DbDate UTCTime
    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr accumulateDate []
    where accumulateDate t acc =
            case t of 
                DbDate d  -> (d:acc)
                _         -> acc

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr accumulate []
    where accumulate t acc =
            case t of 
                DbNumber i  -> (i:acc)
                _         -> acc

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (sum l) / fromIntegral (length l)
                where l = filterDbNumber db

fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

-- Chapter review
stops = "pbtdkg"
vowels = "aeiou"

stsWords = [(s, v, s') | s <- stops, v <- vowels, s' <- stops, s == 'p']

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr ((||) . (==) e) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' e = myAny ((==) e)

myReverse :: [a] -> [a]
myReverse = foldl' (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x l -> f x : l) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr g [] where g x l = if f x then x:l else l

squish :: [[a]] -> [a]
squish = foldr g [] where g x l = x ++ l

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr g [] where g x l = f x ++ l

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy cmp = foldl1 g where g x y = case cmp x y of
                                             GT -> y
                                             _  -> x
                                        