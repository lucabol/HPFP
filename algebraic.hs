{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
import Data.Int
import Data.Char

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | Catapult | ChanceUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Integer Integer deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _       = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _ _) = True
isPlane _           = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _         = error "No manifacture"

data Example = MakeExample  Int deriving Show

newtype Goats = Goats Int deriving (Eq, Show, TooMany)
newtype Cows = Cows Int deriving (Eq, Show)
-- Now we can rewrite our type to be
-- safer, pattern matching in order
-- to access the Int inside our data
-- constructor Goats.

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a where tooMany :: a -> Bool

instance TooMany Int where tooMany n = n > 42

-- Not needed because pragma language start of file
-- instance TooMany Goats where tooMany (Goats n) = n > 43

type Blah = Int

-- instance TooMany Blah where tooMany n = n <34

instance TooMany (Int, String) where tooMany (x, _) = x < 50
instance TooMany (Int, Int) where tooMany (x, y) = x + y < 50

data NumberOrBool =
    Numba Int8
    | BoolyBool Bool
    deriving (Eq, Show)

data Person =
    Person { name' :: String, age :: Int }
    deriving (Eq, Show)

data Fiction = Fiction deriving Show

data Nonfiction = Nonfiction deriving Show

data BookType = FictionBook Fiction | NonfictionBook Nonfiction deriving Show

type AuthorName = String

data Author = Author (AuthorName, BookType)

data GuessWhat = Chickenbutt deriving (Eq, Show)
data Id a = MkId a deriving (Eq, Show)
data Product a b = Product a b deriving (Eq, Show)
data Sum a b = First a | Second b deriving (Eq, Show)
data RecordProduct a b = RecordProduct { pfirst :: a, psecond :: b } deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)
data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
type Farmhouse' = Product NumCow NumPig

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct { pfirst = 42, psecond = 0.00001 }

data OperatingSystem =
    GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgrammingLanguage =
    Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem, lang :: ProgrammingLanguage }
    deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill
    , Mac
    , Windows
    ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer os lang | os <- allOperatingSystems, lang <- allLanguages]   

newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show

-- Pattern match product type

-- FarmerType is a Sum
data FarmerType = DairyFarmer
    | WheatFarmer
    | SoybeanFarmer deriving Show

    -- Farmer is a plain ole product of
-- Name, Acres, and FarmerType
data Farmer = Farmer Name Acres FarmerType deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

-- Pattern match record

data FarmerRec =
    FarmerRec { name :: Name
    , acres :: Acres
    , farmerType :: FarmerType } deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer = case farmerType farmer of
    DairyFarmer -> True
    _ -> False

data Product' a b = a :&: b deriving (Eq, Show)

data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert x Leaf = Node Leaf x Leaf
insert x (Node t1 y t2)
    | x == y = Node t1 x t2
    | x < y  = Node (insert x t1) y t2
    | x > y  = Node t1 y (insert x t2)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
-- acceptance test for mapTree
mapOkay =
    if mapTree (+1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : preorder left ++ preorder right
    
inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = preorder left ++ [a] ++ preorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = preorder left ++ preorder right ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
    if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
    if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "postorder failed check"

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f acc Leaf = acc
foldTree f acc (Node left a right) = foldTree f acc' left
    where
        acc'  = f a acc''
        acc'' = foldTree f acc right

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf xs ys = foldr g True xs where g x b = elem x ys && b

isSubsequenceOf' :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf' xs ys = and $ [ x `elem` ys | x <- xs]

isSubsequenceOf'' :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf'' xs ys = and $ map (\x -> elem x ys) xs

capitalizeWords :: String -> [(String, String)]
capitalizeWords ss = map (\w@(x:xs) -> (toUpper x : xs, w)) $ words ss 
