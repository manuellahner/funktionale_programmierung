
-- Exercise 1
--(1) auf Blatt gemacht

--data [a] = [] | a : [a]

--(2)

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes (x : xs) = (x : xs) : suffixes xs

--(3)
prefixes:: [a] -> [[a]]
prefixes [] = [[]]
prefixes (x : xs) = (x : xs) : prefixes (withoutlast (x : xs))



withoutlast:: [a] -> [a]

withoutlast [] = error "Nicht bei leeren Funktonen anwendbar"
withoutlast (x:[]) = []
withoutlast (x : xs : [])= [x]
withoutlast (x : xs)= x : withoutlast xs

--(4)
--data Either a b = Left a | Right b

menu:: Char -> [a] -> Either String [[a]] 

menu 'p' x = Right (prefixes x)
menu 's' x= Right (suffixes x)
menu n _= Left  ("("++show n++") is not supported, use (p)refix or (s)uffix" )

--Exercise 2
--(1)
data Expr a  = Number a | Plus (Expr a) (Expr a) | Times (Expr a) (Expr a)
  deriving Show



expr1 = Times (Plus (Number (5.2 :: Double)) (Number 4)) (Number 2)
expr2 = Plus (Number (2 :: Int)) (Times (Number 3) (Number 4))
expr3 = Times (Number "hello") (Number "world")
--in diesen Fall kann man expr3 auch so hinschreiben. Wenn man dann aber mit funktionen arbeiten würde, würde ich auf die Typenklasse Num a zurückgreifen um so nur 
--"lösbare" Ergebnise auszuwerten z.B eval:: Num a=> a -> a 


--(2)
numbers :: Expr a -> [a]       -- change the type

numbers (Number e) = [e]
numbers (Plus a b) = (numbers a ++ numbers b)
numbers (Times a b)= (numbers a++ numbers b)



--(3)

eval :: Num a => Expr a -> a     -- change the type

eval (Number a) = a
eval (Plus a b)= (eval a) + (eval b)
eval (Times a b)= (eval a) * (eval b)

--(4)

exprToString:: (Show a) => Expr a -> String

exprToString (Number a)= show a
exprToString (Times (Plus a b) (Plus c d)) = "("++ exprToString(Plus a b) ++") * ("++exprToString(Plus b c)++")"-- beide Therme Plus
exprToString (Times (Plus a b) c)= "("++ exprToString(Plus a b) ++") * "++ exprToString c --erster Therm Plus
exprToString (Times a (Plus b c))= exprToString a ++" * ("++exprToString(Plus b c)++")"  --zweiter Therm Plus
exprToString (Plus a b)= (exprToString a) ++" + " ++ (exprToString b) 
exprToString (Times a b)= (exprToString a) ++" * " ++ (exprToString b)

expr4 = Times (Plus (Number (5.2 :: Double)) (Number 4)) (Plus(Number 2) (Number 3)) 
