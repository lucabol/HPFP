{-# LANGUAGE NoMonomorphismRestriction #-}

module Test where

f :: Int -> String
f = undefined
g :: String -> Char
g = undefined
h :: Int -> Char
h = g . f

data A
data B
data C

q :: A -> B
q = undefined
w :: B -> C
w = undefined
e :: A -> C
e = w . q

data X
data Y
data Z

xz :: X -> Z
xz = undefined
yz :: Y -> Z
yz = undefined
xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun
    deriving Show
data Date = Date DayOfWeek Int

instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Weds Weds = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False

instance Eq Date where
    (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') =
     weekday == weekday' && dayOfMonth == dayOfMonth'

instance Ord DayOfWeek where
        compare Fri Fri = EQ
        compare Fri _ = GT
        compare _ Fri = LT
        compare _ _ = EQ

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    Hello a     == Hello b   = a == b
    Goodbye a   == Goodbye b = a == b
    _ == _ = False

data Person = Person Bool deriving Show
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah
            | Woot deriving (Show, Eq)
settleDown x = if x == Woot
    then Blah
    else x

type Subject = String
type Verb = String
type Object = String
data Sentence =
    Sentence Subject Verb Object
    deriving (Eq, Show)

data Rocks =
    Rocks String deriving (Eq, Show, Ord)

data Yeah =
    Yeah Bool deriving (Eq, Show, Ord)

data Papu =
    Papu Rocks Yeah
    deriving (Eq, Show, Ord)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

newtype Nada = Nada Double deriving (Eq, Show)
instance Fractional Nada where
    (Nada x) / (Nada y) = Nada (x / y)
    recip (Nada n) = Nada (recip n)
    fromRational r = Nada (fromRational r)

instance Num Nada where
    (Nada x) + (Nada y) = Nada (x + y)
    (Nada x) * (Nada y) = Nada (x * y)
    abs (Nada x) = Nada (abs x)
    signum (Nada x) = Nada (signum x)
    fromInteger i = Nada (fromInteger i)
    negate (Nada x) = Nada (negate x)

addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where f = (\n -> n + 1)

addFive = \ x y -> (if x > y then y else x) + 5

mflip f x y = f y x

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer
data User =   UnregisteredUser
            | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name)
          (AccountNumber acctNum))
            = putStrLn $ name ++ " " ++ show acctNum

myUser = Username "callen"
myAcct = AccountNumber 10456

data WherePenguinsLive =
    Galapagos
    | Antarctica
    | Australia
    | SouthAfrica
    | SouthAmerica
    deriving (Eq, Show)

data Penguin =
    Peng WherePenguinsLive
    deriving (Eq, Show)

isSouthAfrica' SouthAfrica = True
isSouthAfrica' _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)

functionC x y =
    case x > y of
        True -> x
        False -> y

nums x =
    case compare x 0 of
    LT -> -1
    GT -> 1
    _ -> 0

tensDigit :: Integral a => a -> a
tensDigit x = d
    where (xLast, _) = x `divMod` 10
          d = xLast `mod` 10

hundredDigit = (`mod` 10) . fst . (`divMod` 100)

foldBool :: a -> a -> Bool -> a
foldBool x y b =
    case b of
    True -> x
    False -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
    | b == True = x
    | b == False = y

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

main = do
    print ((roundTrip 4)::Int)
    print (id 4)