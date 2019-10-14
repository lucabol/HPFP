import Data.List

notThe :: String -> Maybe String
notThe "The" = Nothing
notThe "the" = Nothing
notThe s     = Just s

replaceThe :: String -> String
replaceThe = intercalate " " . reverse . go [] . words 
    where
        go a k = foldl f a k
        f acc v = case notThe v of
                    Nothing -> "a":acc
                    Just w  -> w:acc

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel l =
    let f ("The", s) b = if isVowel (head s) then b + 1 else b
        f ("the", s) b = if isVowel (head s) then b + 1 else b
        f (_,_)      b = b
        ws             = words l
        wz             = zip ws (tail ws)   
    in foldr f 0 wz

countVowels :: String -> Integer
countVowels = toInteger . length . filter isVowel

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s =
    let vow = (length . filter isVowel) s
        con = length s - vow
    in if vow > con then Nothing else Just $ Word' s

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat x
    | x < 0 = Nothing
    | x >= 0 = Just (go x)
            where go 0 = Zero
                  go x = Succ (go (x - 1))  