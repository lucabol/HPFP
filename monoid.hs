import Data.Monoid
import Test.QuickCheck
import Control.Monad

data Booly a = False' | True' deriving (Eq, Show)

instance Semigroup (Booly a) where
    False' <> _    = False'
    _ <> False'    = False'
    True' <> True' = True'

instance Monoid (Booly a) where
    mempty = False'

data Optional a =
    Nada
    | Only a
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    Only a <> Only b = Only (a <> b)
    Only a <> _      = Only a
    _      <> Only b = Only b
    _      <> _      = Nada

instance Monoid a => Monoid (Optional a) where
    mempty = Nada

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj = mconcat [
    e, "! he said ",
    adv, " as he jumped into his car",
    noun, " and drove off with his ",
    adj, " wife"]

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = frequency [ (1, return Fools), (1, return Twoo) ]

instance Semigroup Bull where
    _ <> _ = Fools

instance Monoid Bull where
    mempty = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

main :: IO ()
main = do
    quickCheck (monoidAssoc :: BullMappend)
    quickCheck (monoidLeftIdentity :: Bull -> Bool)
    quickCheck (monoidRightIdentity :: Bull -> Bool)

newtype First' a = First' {getFirst':: Optional a} deriving (Eq, Show)

instance Semigroup (First' a) where
    First' (Only a)  <> _               = First' (Only a)
    First' Nada      <> First' (Only a) = First' (Only a)
    _                <> _               = First' Nada

instance Monoid (First' a) where
    mempty = First' Nada

type FirstMapped = First' String -> First' String -> First' String -> Bool

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
        a <- arbitrary
        frequency [ (3, return $ First' (Only a)),
                    (1, return $ First' Nada) ]

main' :: IO ()
main' = do
    quickCheck (monoidAssoc :: FirstMapped )
    quickCheck (monoidLeftIdentity :: First' String -> Bool)
    quickCheck (monoidRightIdentity :: First' String -> Bool)

data Trivial = Trivial deriving (Eq, Show)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

mainTrivial :: IO ()
mainTrivial = do
    quickCheck (semigroupAssoc :: TrivialAssoc)
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    Identity a <> Identity b = Identity (a <> b)

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty
    
instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

mainId :: IO ()
mainId = do
    quickCheck (semigroupAssoc :: Identity String -> Identity String -> Identity String -> Bool)
    quickCheck (monoidLeftIdentity :: Identity String -> Bool)
    quickCheck (monoidRightIdentity :: Identity String -> Bool)
        
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    Two a b <> Two a' b' = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty  = Two mempty mempty
    
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

type TwoSI = Two String (Sum Int)
mainTwo :: IO ()
mainTwo = do
    quickCheck (semigroupAssoc :: TwoSI -> TwoSI -> TwoSI -> Bool)
    quickCheck (monoidLeftIdentity :: TwoSI -> Bool)
    quickCheck (monoidRightIdentity :: TwoSI -> Bool)

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    BoolConj True <> BoolConj True = BoolConj True
    _             <> _             = BoolConj False

instance Monoid BoolConj where
    mempty = BoolConj True
        
instance Arbitrary BoolConj where
    arbitrary = do
        a <- arbitrary
        return $ BoolConj a

mainBj :: IO ()
mainBj = do
    quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (monoidRightIdentity :: BoolConj -> Bool)

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
    Combine f <> Combine g = Combine (\a -> f a <> g a)

instance (Monoid a, Monoid b) => Monoid (Combine a b) where
    mempty = Combine mempty
    
data Validation a b = Failure' a | Success' b deriving (Eq, Show)

instance (Semigroup b, Semigroup a) => Semigroup (Validation a b) where
    Success' b <> Success' b' = Success' (b <> b')
    Failure' a <> Failure' a' = Failure' (a <> a')
    Failure' a <> _           = Failure' a
    _          <> Failure' a  = Failure' a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [ (1, return $ Success' a),
                    (1, return $ Failure' b) ]

type ValSI = Validation String (Sum Int)


mainVa :: IO ()
mainVa =
    quickCheck (semigroupAssoc :: ValSI -> ValSI -> ValSI -> Bool)

newtype Mem s a = Mem { runMem :: s -> (a,s) }
 
instance Semigroup a => Semigroup (Mem s a) where
    Mem f <> Mem g = Mem reduce where
        reduce s = let (fa, fs) = f s
                       (ga, gs) = g fs in
                    (fa <> ga, gs) 

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem (\x -> (mempty, x))

f' = Mem $ \s -> ("hi", s + 1 :: Int)

type MemTest = Mem String Int

mainMem = do
    print $ runMem (f' <> mempty) 0
    print $ runMem (mempty <> f') 0
    print $ (runMem mempty 0 :: (String, Int))
    print $ runMem (f' <> mempty) 0 == runMem f' 0
    print $ runMem (mempty <> f') 0 == runMem f' 0