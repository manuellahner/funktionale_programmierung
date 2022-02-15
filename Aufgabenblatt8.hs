
--Exercise 2:
--Exercise 3:

dig2int :: [Integer] -> Integer 
dig2int []=0
dig2int (x:xs)= x +10 *dig2int xs


dig2intFold:: [Integer]-> Integer 

dig2intFold []= 0
dig2intFold(x:xs)=(\x y-> x+10*x) x(xs)