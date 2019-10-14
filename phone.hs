import Data.List
import Data.Char
import Data.Maybe
import Data.Map

type DaPhone = [String]

phone :: DaPhone
phone = [" 0", "1", "abc2", "def3", "ghi4", "jkl5", "mno6", "pqrs7", "tuv8", "wxyz9"]

convo :: [String]
convo =
    ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"]

type Digit = Char
type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps p c =
    let clow = toLower c
        smay = find (elem clow) p
        s    = fromJust smay
        low  = [(last s, fromJust (elemIndex clow s) + 1)]
    in if isUpper c then ('*', 1) : low else low

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead p = Data.List.foldl (++) [] .Data.List.map (reverseTaps p)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . Data.List.map snd

mostPopularLetter :: String -> Char
mostPopularLetter = fst . maximumBy (\(_,i) (_,j) -> compare i j) . toList . fromListWith (+) . cellPhonesDead phone