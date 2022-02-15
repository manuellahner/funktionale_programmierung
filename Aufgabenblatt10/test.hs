countletters:: Char  -> String -> Int

countletters a []=0
countletters a (x:xs)= if (a==x) then (1+ countletters a xs) else countletters a xs



test=  "0123456\nOX.....\n.O.....\n..XX..\n..X.O..\n.X...O.\nO.....X\n\nPlayer X to go"

reversepc:: String -> [[Int]]
reversepc str= map stringToTiles $ take 6(drop 1 $lines str) 

reverseTile:: Char -> Int 
reverseTile 'X'= 1
reverseTile 'O'=2
reverseTile '.'=0

stringToTiles:: String -> [Int]
stringToTiles str= map reverseTile str


myarr=reversepc test
--diagonalen anschauen.

reversereverselist:: [[Int]]->[[Int]]
reversereverselist a= reverse$ map reverse a

alldiagonalsl::Int->[[Int]]->[Int]
alldiagonalsl sv []=[]
alldiagonalsl sv x =  (head$drop sv $ head  x) : (alldiagonalsl (sv+1) (tail  x) )


alldiagonalsfirst::Int->[[Int]] ->[[Int]]

alldiagonalsfirst a []=[]
alldiagonalsfirst x a= (alldiagonalsl x  a) : (alldiagonalsfirst x  (drop 1  a)) 

alldiagonalssecond::Int->[[Int]] ->[[Int]]
alldiagonalssecond a []=[]
alldiagonalssecond x a= alldiagonalsfirst x (reversereverselist a)
finaldiag::[[Int]]->[[Int]]
finaldiag  a= alldiagonalsfirst 0 a ++alldiagonalssecond 0 a ++ alldiagonalsfirst 0 (reverse a) ++alldiagonalssecond 0 (reverse a)
