import GHC.Stack.Types (CallStack(EmptyCallStack))


data Menu=Item String String  deriving Show


data Menulist= Emptymenu
    |One Menu
    |Cons Menu Menulist     
 deriving Show

disj:: Bool-> Bool -> Bool

disj True a= a
disj _ _ = False

sumlist:: [Int]-> Int
sumlist x= foldr (+) 0 x


double2nd:: [Int]->[Int]

double2nd []= []

double2nd (x:xs:xxs)= x:2*xs:double2nd xxs
double2nd (x:[])= x:[]

data Expr=
    Number Integer 
    | Var String 
    | Plus Expr Expr
    |Negate Expr
  

data Assignment= EmptyA | Assign String Integer Assignment


ite:: Bool -> Integer -> Integer -> Integer 
ite True a b= a
ite False _ b = b

lookupA :: Assignment -> String ->Integer
lookupA EmptyA x = 0
lookupA (Assign a b c) d= if (a==d) then b else lookupA c d


myAssn= Assign "x" 1 (Assign "x" 2 (Assign "y" 3 EmptyA))


myeval::Assignment-> Expr -> Integer 
myeval _ (Number a) = a
myeval a (Plus b c)= myeval a b + myeval a c
myeval l (Var d)= lookupA l d
myeval l (Negate s)= -myeval l s


data Expr'=
    Number' Integer 
    | Var' String 
    | Plus' Expr Expr
    |Negate' Expr
    |Let' String Expr' Expr'

myeval'::Assignment-> Expr' -> Integer 
myeval' _ (Number' a) = a
myeval' a (Plus' b c)= myeval a b + myeval a c
myeval' l (Var' d)= lookupA l d
myeval' l (Negate' s)= -myeval l s
myeval' assn (Let' s e1 e2) = myeval' (Assign s (myeval' assn e1) assn) e2

divides :: Integer -> Integer -> Bool
divides x y = mod y x == 0


dividesRange :: Integer -> Integer -> Integer-> Bool


dividesRange n l u
    | l > u = False
    | otherwise = divides l n || dividesRange n (l + 1) u


prime :: Integer -> Bool 
  
prime x= x>=2 && not (dividesRange x 2 (sqrtIntBisect x)) 

generateprime:: Integer -> Integer

generateprime x=gen (10^(x-1) + 1) where
    gen n 
     | prime n=n
     | otherwise= (gen n+2)


sprtInt:: Integer -> Integer 
sprtInt x= main 0 where
    main a
     | a*a > x =  a-1
     | otherwise = main (a+1)

sqrtIntBisect :: Integer -> Integer
sqrtIntBisect x = s 0 x where
    s l u -- we have lower and bounds l and u satisfying l^2 <= x <= u^2
        | u - l > 1 =
         let m = div (u + l) 2;
             m2 = m * m
           in if m2 > x then s l m
                else if m2 < x then s m u
                else m
        | u == l = l
        | otherwise = if u^2 <= x then u else l

toInt :: Float -> Integer
toInt = round