module Print1 where

main :: IO ()
main =  do
    putStrLn "Count to four for me:"
    putStr "one, two"
    putStr ", three, and"
    putStrLn " four!"

data Mood = Blah | Woot deriving Show

changeMood Blah = Woot
changeMood _    = Blah