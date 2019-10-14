module Cipher where

import Data.Char

shiftChar ::  Int -> Char -> Char
shiftChar n c =
    let o = ord c
        s = o - ord 'a'
        r = s + n
        k = r `mod` 26
        x = k + ord 'a'
    in chr x
    
caesar :: Int -> String -> String
caesar n = map (shiftChar n)

unCaesar :: Int -> String -> String
unCaesar n = map (shiftChar (-n))

type Keyword = String

repeatKeyword :: Keyword -> String
repeatKeyword = foldr (++) "" . repeat

vigenere :: Keyword -> String -> String
vigenere k s =
    let rl = repeatKeyword k
        base = ord 'a'
        shiftCalc kc = ord kc - base
        f :: (String, String) -> Char -> (String, String)
        f (rl', acc) c = case c of
                            ' ' -> (rl', c : acc)
                            _   -> (tail rl', shiftChar (shiftCalc (head rl')) c : acc)
        (_, res) = foldl f (rl, []) s
        in reverse res




