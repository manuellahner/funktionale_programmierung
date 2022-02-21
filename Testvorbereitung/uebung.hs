




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

suffixes:: [a]->[[a]]
suffixes[]=[[]]
suffixes x= x : suffixes(tail x)

prefixes::[a]->[[a]]
prefixes []= [[]]
prefixes a= a: prefixes (init a)


menu:: Char-> [a]-> Either ([[a]]) (String)
menu a b
    |a=='p' = Left (prefixes b)
    |a=='s' = Left (suffixes b)
    |otherwise = Right ("("++ [a] ++ ") is not supported, use (p)refix or (s)uffix")


data Myexpr a= 
     Numba a
    | Plu (Myexpr a) (Myexpr a)
    |Times (Myexpr a) (Myexpr a)
    deriving Show
    
numbers::Num a=>Myexpr a -> [a]
numbers (Numba x) = x:[]
numbers (Plu b c)= numbers b ++ numbers c
numbers (Times b c)=numbers b ++ numbers c

eval::Num a=> Myexpr a -> a
eval (Numba a)= a
eval (Plu b c)=eval b + eval c
eval (Times b c)= (eval b)* (eval c)


mergeList::[a]->[b]->[(a,b)]
mergeList _ []= []
mergeList [] _ = []
mergeList (a:ax) (b:bx)= (a,b):mergeList ax bx


today=(16,2,2022)

calculateage::(Int,Int,Int)-> Int
calculateage (a,b,c)
   |(a>=16) && (b>=2) = (2022-c-1)
   |otherwise =  (2022-c)


convertDatesToAges:: [(String,(Int,Int,Int))]->[(String,Int)]
convertDatesToAges []=[]
convertDatesToAges ((s,d):xs)=(s,calculateage d): convertDatesToAges xs



getOtherPairValue:: (String, Int)-> Either String Int-> Either String Int

getOtherPairValue (a,b) (Left c) = Left a
getOtherPairValue(a,b) (Right c) = Right  b

fstList:: [(a,b)]-> [a]
fstList []= []
fstList ((a,b):xs)= a:fstList xs

lengthSumMax ::(Num a, Ord a)  => [a]->(Int,a,a)
lengthSumMax a= (length a, sum a, maximum a)


data Rat = Rat Integer Integer 

normalize:: Rat -> Rat
normalize (Rat a b)
   |abs(a)>abs(b)=kuerzen a b (abs b) 
   |otherwise = kuerzen a b (abs a)
  where 
     kuerzen x y z
        |(abs(x)>abs(y))= if x`mod`z==0 && y`mod`z==0 then (Rat (x`div`z) (y`div`z)) else kuerzen x y (z-1)
        |otherwise = if x`mod`z==0 && y`mod`z==0 then Rat (x`div`z) (y`div`z) else kuerzen x y (z-1)

instance Eq Rat where
    Rat a1 b1 == Rat a2 b2= a1*b1==a2*b2

instance Ord Rat where
    a <= b = myins where
               Rat r1 s1 = normalize a
               Rat r2 s2 = normalize b
               myins = r1*s2<=r2*s2

instance Show Rat where
    show (Rat a b)= if b==1 then show a else show a ++"/"++show b

data Ingredient= Ingredient String Float Unit
data Unit= ML| PC| G deriving Show

instance Show Ingredient where
    show(Ingredient a b c)= show b ++" "++show c ++ " of "++ a++", costs: "++show(getPrice(Ingredient a b c))++" EUR"


class Price a where
    getPrice:: a -> Float

instance Price Ingredient where
    getPrice(Ingredient s x ML)= x* 0.12/100
    getPrice(Ingredient s x G)= x* 0.095/100
    getPrice(Ingredient s x PC)= x*0.75/100


data Recepie= Recepie [Ingredient]

instance Price Recepie where
    getPrice (Recepie [])=0
    getPrice (Recepie (x:xs))=getPrice x + getPrice (Recepie xs)

instance Show Recepie where
    show (Recepie [])=""
    show (Recepie (x:xs))= "-"++ show x ++ show (Recepie xs)

testing= Ingredient "Zucker" 70 G
testing1= Ingredient "Wasser" 20 ML
testing2= Ingredient "Zitronen" 2 PC
testing3= Recepie[testing, testing1, testing2]
ing1 = Ingredient "Milk" 200 ML
ing2 = Ingredient "Sugar" 200 G
ing3 = Ingredient "Egg" 3 PC
recipetest = Recepie[ing1, ing2, ing3]


--Aufgabenblatt 8

fan :: (a->Bool)->[a]->[[a]]
fan p []=[]
fan p l@(arr:arrx) 
  |(p arr)==True = takefirst (myfan l): fan p (takesecond(myfan l)) 
  |otherwise  = takefirst (myfnnot l): fan p (takesecond(myfnnot l))   
 where
    myfan xs= span p xs
    myfnnot xs= span (not.p) xs
    takefirst (a,b)= a
    takesecond (a,b)=b


splitOnNumbers= fan (\x-> x=='1' ||x=='2'||x=='3'||x=='4'||x=='5'||x=='6'||x=='7'||x=='8'||x=='9'||x=='0')

splitBy::(a->Bool)->[a]->[[a]]
splitBy p (x:xs)
  |p x== True = takesecond(fan p (x:xs))
  |otherwise = takefirst(fan p (x:xs)) 
 where
    takesecond[]=[]
    takesecond (x:xs:xxs)= xs: takesecond xxs
    takesecond (x:[])= []
    
    takefirst []=[]
    takefirst (x:xs:xxs)= x: takefirst xxs
    takefirst (x:[])=x:[]


--foldr funktionen

dig2int::[Integer]->Integer 
dig2int a= foldr (\x acc -> x+ 10 *acc)0 a

suffs::[a]->[[a]]

suffs a= foldr(\x acc ->(x:head(acc)):acc) [[]] a



--Aufgabenblatt 10

shift:: Int -> Char -> Char 
shift a b
 |fromEnum b<97 || fromEnum b>122= b
 |fromEnum (b) + a >122= shift (fromEnum(b)+a-123) 'a'
 |otherwise= toEnum(fromEnum(b)+a)


encode:: Int ->String -> String 
encode x a= map (shift 5) a



count:: Char-> String -> Int 
count a x= mycount 0 a x  where
    mycount acc c []= acc
    mycount acc c (s:sx)= if s==c then mycount (acc+1) c sx else mycount acc c sx
