--Exercise 1
 --1

dividesrange :: Integer  -> Integer -> Integer -> Bool 
    

dividesrange a b c= case b==c of
   True -> case (mod a b ==0) of
      True -> True
      False -> False
     
   False -> case mod a b == 0 of
      True -> True
      False -> dividesrange a (b+1) c
      
   
test1= dividesrange 55 6 8
test2= dividesrange 55 5 6

--2
prime:: Integer -> Bool

prime x= mydivs 2 x where
   mydivs y x=
      case (y<x) && (x>=2) of
         True -> if (mod x y == 0) then False else mydivs (y+1) x
         False -> case x==y of
            True-> True 
            False-> False

expected= "[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]"
testPrime =  filter prime [0..100]
testPrimef= filter prime [0..100]

--3
generateprime :: Integer -> Integer --funktioniert aber sehr langsam

generateprime x= myprime (10^(x-1))  ((10^x)-1) where
   myprime y z= 
      case (y<=z) of
       True -> case prime y of
          True -> y
          False -> myprime (y+1) z
       False -> 0

--4 fasterprime Idee: immer nur ungerade Zahlen nehmen dann dauert es nur halb so lang und ich gehe davon aus, dass es immer eine primzahl zwischen den Werten gib
   -- also fällt auch das kontrollieren ob es kleiner gleich z ist weg

fasterprime:: Integer -> Integer 
fasterprime x= myprime (10^(x-1)+1) where
     myprime x =
        case primef x of
           True -> x
           False -> myprime (x+2) 

-- mögliche andere Verbesserungen: myprime schneller machen oder mit guarded equations falls diese schneller wie case of gehen

primef :: Integer -> Bool
primef x
 | x==1 = False
 |mod x 2 == 0 = False 
 |otherwise = mydivs 3 x where
    mydivs y x=
      case (y<x) && (x>=2) of
         True -> if (mod x y == 0) then False else mydivs (y+2) x
         False -> case x==y of
             True-> True 
             False-> False



--Wurzelfunktion
root x= myroot x where
 myroot y=
  case y*y>x of
   True-> myroot (y-1)
   False -> y
  



--Exercise 2 Heron Method

heron :: Double  -> [Double] 
heron x = myheron x x where
  myheron x y
    |x == 0 = [0]
    |x == myfun = [x] 
    |otherwise = x : (myheron myfun y) where 
       myfun = 1/2 * ( x + (y / x))    



--Exercise 3 Fibbonacci

--1

fib :: Integer -> Integer
fib x
  | x< 2 = x
  | otherwise = fib (x - 1) + fib(x - 2)



fib' :: Integer -> Integer
fib' x
 |x==0 =0
 |x<=2 =1
 |otherwise  = case  (odd x) of
   True -> (y ^ 2) + z ^ 2 where
      y = fib' (div x 2)
      z =  fib' ((div x 2) + 1)
   False -> ((z * 2)-y)*y where 
         y = fib' (div x 2)
         z =  fib' ((div x 2) + 1)

--schneller, weil nicht 2 mal fast alles rekursiv durchgegangen werden muss für eine Zahl

--2 fibfast leider nicht geschafft
 
    