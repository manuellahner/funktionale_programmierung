--Exercise 1
--(1)

data Rat = Rat Integer Integer  

norm:: Rat -> Rat
-- wenn eines, Zähler oder Nenner negativ sind, wird immer der Zähler bei mir negativ gemacht

norm (Rat a b)
 |a<0 && b<0 = Rat (-1*(div a (gcd a b))) (-1*(div b (gcd a b))) --beides negativ
 |a>0 && b<0 = Rat (-1*(div a (gcd a b))) (-1*(div b (gcd a b))) --a positiv b negativ
 |a<0 && b>0 =  Rat (div a (gcd a b)) (div b (gcd a b)) -- a negativ b positiv
 |b==0= Rat 0 0 --Wie eine Fehlermeldung
 |otherwise= Rat (div a (gcd a b)) (div b (gcd a b))      --beides positiv 


myrat= Rat 8 4
myrat1= Rat (-8) (-4)

myrat2= Rat (-8) 4

--(2)

instance Eq Rat where
    (Rat a b) == (Rat c d) = x1== x2 && y1==y2
        where  Rat x1 y1= norm (Rat a b)
               Rat x2 y2= norm (Rat c d)

--(3)


instance Show Rat where 
    show (Rat a b)= if (y1==1) then show x1 else show x1++"/"++show y1
        where Rat x1 y1= norm (Rat a b)

--(4)
instance Num Rat where
    (+) (Rat a b) (Rat c d)= norm(Rat((d*a)+(b*c))(b*d))
    (-) (Rat a b) (Rat c d)= norm(Rat((d*a)-(b*c))(b*d))
    (*) (Rat a b) (Rat c d) = norm(Rat(a*c)(b*d))
    negate (Rat a b)= Rat (negate(a)) b
    abs (Rat a b)= Rat (abs a) (abs b)
    signum (Rat a b)
        |(a>0 && b>0) || (a<0 && b<0)= Rat 1 1 --wenn positiv Rat 1 1 wenn negativ Rat 0 0
        |otherwise = Rat (-1) 1                        --abs x*signum x=x
    fromInteger a= Rat (fromInteger a) 1





--Exercise 2
--(1)

data MyUnit= 
    ML 
    |G 
    |PC  


data Ingredients= Ingredients String Float MyUnit

instance Show Ingredients where
    show (Ingredients a b ML)= show b++ " ML of "++ a ++ " ,cost: "++show(getPrice(Ingredients a b ML))++" EUR "
    show (Ingredients a b G)=  show b++ " g of "++ a ++ " ,cost: "++show(getPrice(Ingredients a b G))++" EUR "
    show (Ingredients a b PC)=  show b++ " PC of "++ a ++ " ,cost: "++show(getPrice(Ingredients a b PC))++" EUR "

testing= Ingredients "Zucker" 70 G
testing1= Ingredients "Wasser" 20 ML
testing2= Ingredients "Zitronen" 2 PC

--(2)
class Price a where
    getPrice :: a -> Float 

instance Price Ingredients where
    getPrice (Ingredients a b ML)= 0.12 *b / 100
    getPrice (Ingredients a b G)= 0.095*b/100
    getPrice (Ingredients a b PC)= 75*b/100


--Ausbesserung der Showklasse siehe Aufgabe 1



--(3)
data Recipe= Recipe [Ingredients]

instance Price Recipe where
    getPrice (Recipe[])=0
    getPrice (Recipe (Ingredients a b c : xs))= getPrice(Ingredients a b c)+ getPrice(Recipe xs)

instance Show Recipe where
    show (Recipe [])=""
    show (a)= myshow a ++ "Price of the Recepi: " ++show (getPrice a)
   
     
myshow:: Recipe -> String 
myshow (Recipe[])=""
myshow (Recipe(Ingredients a b c :xs))= show (Ingredients a b c) ++ " - "++myshow (Recipe xs)




testing3= Recipe[testing, testing1, testing2]
ing1 = Ingredients "Milk" 200 ML
ing2 = Ingredients "Sugar" 200 G
ing3 = Ingredients "Egg" 3 PC
recipetest = Recipe[ing1, ing2, ing3]





     