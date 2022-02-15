--Aufgabenblatt 9
import Data.List -- for the test
--Exercise 1
--(1)
shift:: Int -> Char -> Char

shift x a
 |(fromEnum a)>=97&&(fromEnum a)<=122 = if (((fromEnum a)+x)>122) then toEnum((fromEnum a)+x-122+96) else toEnum((fromEnum a)+x)
 |otherwise= a



encode:: Int -> String -> String

encode x y = [shift x n|n <- y]

--(2)

--(a)
count:: Char -> String -> Int 

count a b = length [n | n <- b,n==a]

percent :: Int-> Int -> Float 
percent a b= (fromIntegral a :: Float)/(fromIntegral b ::Float)*100

--(b)

freqs:: String -> [Float]

freqs a= [percent (count n a) (length a)|n<- "abcdefghijklmnopqrstuvwxyz" ]

--(c)
mysum::[Float]-> [Float]->[Float]
mysum [] []=[]
mysum (o:ox) (e:ex)= ((o-e)*(o-e))/e : mysum ox ex
mysum _ _= []


chisqr:: [Float] -> [Float] -> Float 

chisqr ox ex= sum(mysum ox ex)

--(d)

rotate :: Int -> [a] -> [a]

rotate n a=  (drop n a) ++ (take n a)


pairzip:: [a] -> Int ->  [(a,Int)]
pairzip (x:xs) i= (x,i): pairzip xs (i+1)
pairzip _ _=[]

takesectup:: (a,b)->b
takesectup (a,b)= b

takefirsttup:: (a,b)->a
takefirsttup (a,b)= a


positions:: Eq a => a -> [a]-> [Int]
positions a b= [takesectup n | n <- (pairzip b 0),takefirsttup(n)==a]


--3
freqList = [8.2, 1.5, 2.8, 4.3, 13, 2.2, 2, 6.1, 7, 0.15, 0.77, 4, 2.4, 6.7,
            7.5, 1.9, 0.095, 6, 6.3, 9.1, 2.8, 0.98, 2.4, 0.15, 2, 0.074]

crack::String->String 
mychisqr a= chisqr a freqList
crack a=  encode (minimum (positions mymin (map mychisqr [(rotate (26-n) (freqs a))|n<-[0..25]]))) a
    where  mymin=(minimum (map mychisqr [(rotate (26-n) (freqs a))|n<-[0..25]]))
--Exercise 2:
test1= crack "rkcuovv sc pex"
--(1)
fact :: Integer -> Integer 

fact 0 = 1
fact a= a*fact(a-1)

binom:: Integer -> Integer -> Integer 

binom n k= (fact n) `div` (fact k * fact(n-k))

--(2)
bernulli:: Integer -> Rational
bernulli 0=1
bernulli n = sum[ fromInteger (binom n k) * bernulli(k) / fromInteger (k-n-1)|k<-[0..(n-1)]]
        
--(3) -- nicht angekreuzt, stimmt nicht
bernoullis :: Integer -> [Rational]


bernoullis n = [bernulli x|x<-[0..n]]

--(4)


ispositive:: Rational  -> Bool 
ispositive a= if a>=0 then True  else False

isnegative::Rational  -> Bool
isnegative a= if a<0 then True else False

alllisttrue::[Bool]->Bool 
alllisttrue []=True
alllisttrue (x:xs)= if x==False then False else alllisttrue xs 

--ich gehe hier davon aus dass immer ein Wert größer als 3 eingegeben wird
check1:: Integer -> Bool 
check1 n= alllisttrue[isnegative (bernulli x)|x<-[4..n], x `mod` 4 ==0]


check2:: Integer -> Bool
check2 n= alllisttrue[ispositive (bernulli x)|x<-[4..n], x-2 `mod` 4 ==0]



--Tests
tests = do
  test "1.1  " "\"mjwj nx fs jcfruqj.\"" (encode 5 "here is an example.")
  test "1.2a " "2" (count 'e' "example")
  test "1.2a'" "33.333336" (percent 1 3)
  test "1.2b " "[10.0,20.0,30.000002,40.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]" (freqs "abbcccdddd")
  test "1.2c " "25.0" (chisqr [50,50,0] [40,40,20])
  test "1.2d " "[4,5,1,2,3]" (rotate 3 [1,2,3,4,5])
  test "1.2d'" "[0,2,3]" (positions 3 [3,1,3,3])
  test "1.3  " "\"test\"" (crack (encode 10 "test"))
  test "2.1a " "[1,1,2,6,24,120]" (map fact [0..5])
  test "2.1b " "[1,5,10,10,5,1]" [binom 5 k | k <- [0..5]]
  test "2.2  " "[1 % 1,(-1) % 2,1 % 6,0 % 1,(-1) % 30,0 % 1,1 % 42]" (map bernulli [0..6])
  test "2.3  " "[1 % 1,(-1) % 2,1 % 6,0 % 1,(-1) % 30,0 % 1,1 % 42]" (bernoullis 6)
  test "2.4a " "True" (check1 10)
  test "2.4b " "True" (check2 10)

test name e c = do
  putStr ("*** " ++ name ++ ": ")
  if show c == e then putStrLn "OK"
  else putStrLn ("ERROR; expected '" ++ e ++ "', but found '" ++ show c ++ "'")
































