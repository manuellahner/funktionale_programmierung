
--Exercise 1
--Auf Blatt erledigt

--Exercise 2

--1)
disj :: Bool -> Bool -> Bool
disj False  False = False     --nur Falsch wenn beide falsch sind
disj _ _ = True



--2)
data List = Empty | Cons Integer List deriving Show
testlist1= Empty
testlist2= Cons 1 (Cons 2 (Cons 7 Empty))
testlist3=Cons 8 (Cons 22 (Cons 14 Empty))

sumlist :: List -> Integer
sumlist Empty = 0
sumlist (Cons x xs) = x + sumlist xs

--3)
double2nd :: List -> List

double2nd Empty = Empty

double2nd (Cons x Empty)= Cons x Empty


double2nd (Cons x (Cons y list)) = Cons x (Cons (y * 2) (double2nd list))

--Exercise 3

--1)
ite :: Bool-> Integer -> Integer -> Integer

ite True x _ =x 
ite False _ y = y

--2)

data Assignment = EmtyA | Assign String Integer Assignment deriving Show

myAssn= Assign "x" 1 (Assign "x" 2 (Assign "y" 3 EmtyA))
  
lookupA :: Assignment -> String -> Integer

lookupA (Assign a b c) d = ite (a==d) b (lookupA c d)
lookupA _ _ = 0


--3)
data Expr =
    Number Integer
    |Var String
    |Plus Expr Expr
    |Negate Expr

eval :: Assignment -> Expr -> Integer 
eval a (Plus x y)= eval a x + eval a y
eval a (Var x)= lookupA a x
eval a (Negate x)= -(eval a x)
eval a (Number x) = x

testauswertung= eval myAssn (Plus (Negate (Var "y")) (Number 45)) --(-3+45)

--4) --nicht geschafft













