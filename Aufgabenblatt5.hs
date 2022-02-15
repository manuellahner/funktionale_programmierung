
--Exercise 1
 
--(1)
mergeList:: [a] -> [b] -> [(a,b)]

mergeList (x:[]) (y:ys)= [(x,y)]
mergeList (x:xs) (y:[])= [(x,y)]
mergeList (x:xs) (y:ys)= (x,y) : mergeList xs ys
mergeList [] y= []
mergeList x []= []

--(2)

--Heutiges Datum:
day=10
month=11
year=2021

calculateAge::(Int,Int,Int) -> Int 

calculateAge(d,m,y)=ite (d > day && m == month || m > month) (2021 -1 - y) (2021 -y)


ite :: Bool -> a -> a -> a
ite True x y = x
ite False x y = y



--(3)
convertDatesToAges:: [(String,(Int,Int,Int))] -> [(String,Int)]

convertDatesToAges []= []
convertDatesToAges ((x,(d,m,y)):[])= (x,calculateAge(d,m,y)):[]
convertDatesToAges ((x,(d,m,y)):xs)= (x,calculateAge(d,m,y)):convertDatesToAges xs

mydates :: [(String, (Int, Int, Int))]
mydates=[("PersonA"::String,(2::Int,12,2001)),("PersonB",(3,3,2003)),("PersonC",(10,11,2021))]

--(4)
--data Maybe a = Nothing | Just a
--data Either a b = Left a | Right b

getOtherPairValue::(String ,Int) -> Either String Int -> Maybe (Either String Int)

getOtherPairValue (a,b) (Left s)= ite (a==s) (Just (Right b)) Nothing 
getOtherPairValue (a,b) (Right i)= ite(b==i) (Just (Left a )) Nothing 

test1= getOtherPairValue ("Manuel",19) (Left "Manuel")
test2= getOtherPairValue ("Manuel",19) (Right 19)

--Exercise 2
--(1)
--Erklärung auf Blatt
addPair:: Num a => (a,a)-> a --Num weil addiert wird
addPair(x,y)= x+y

addList:: Num a=> [(a,a)] ->[a]-- Num weil auch hier addiert wird

addList [] =[]
addList (x:xs)= addPair x : addList xs

testlist=[(1,2),(3,4),(5,6)]

--(2)
--auf Blatt gelöst
test = addList ((1,2):(2,1):[])

--(3)

fstList :: [(a,b)] -> [a]

fstList [] = []

fstList ((a,b) : c )= a : (fstList c)

t1= fstList [(1,'a'),(2,'b'),(3,'c')] 
t2= fstList [("hello","world")]
t3 =fstList []

--(4)

lengthSumMax :: (Num a, Ord a) => [a] -> (Int, a, a)

lengthSumMax [] = (0,0,0)
lengthSumMax a = ((len a),(sumlist a),(maxelement a))

len:: [a] -> Int 
len [] = 0
len (a : b) = 1 + (len b)

sumlist :: (Num a)=> [a] -> a 

sumlist[] = 0
sumlist (x : xs)= x + (sumlist xs)

maxelement:: (Ord a, Num a) => [a] -> a

maxelement []= 0

maxelement (x: xs)= 
    let y= maxelement xs 
     in if(x>y) then x else y


lsmtest= lengthSumMax [0,1,0,2,0]