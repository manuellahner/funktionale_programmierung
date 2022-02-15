import Data.Char -- useful for Exercise 1

-- Exercise 1

--(1),(2) auf Blatt gelöst

--(3)

stringToUpperTail :: String -> String
stringToUpperTail n = aux [] n where
    aux acc []= acc
    aux acc (x:xs) = aux (acc ++ [toUpper x]) xs


stringToUpperGuarded :: String -> String
stringToUpperGuarded []=[]
stringToUpperGuarded (x:xs) = toUpper x : stringToUpperGuarded xs

--Exercise 2
-- nicht gemacht


