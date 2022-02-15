module Logic(State, Move, Player,
  startPlayer,initState, showPlayer, showState,
  winningPlayer, validMoves, dropTile,otherPlayer, countletters,reversepc, stringToState) where
import GHC.ST (ST(ST))


type Tile   = Int   -- 0, 1, or 2        3 arten von Spielsteinen . 1 X
type Player = Int   -- 1 and 2
type Move   = Int   -- column number
data State = State Player [[Tile]]  -- list of rows

empty :: Tile
empty = 0         --empty heist dass der Spielstein . eingetragen wird. Also ist das Feld noch zu belegen  

numRows, numCols :: Int
numRows = 6               --6 Zeilen und 7 spalten
numCols = 7

startPlayer :: Int -> Player   -- startspieler wird 1 zugewiesen
startPlayer a=  a

initState :: Player ->State
initState pl = State pl
  (replicate numRows (replicate numCols empty))   --6x7 Matrix mit . wird erzeugt  

validMoves :: State -> [Move]
validMoves (State _ rows) =
  map fst . filter ((== empty) . snd) . zip [0 .. numCols - 1] $ head rows
-- gibt die Indizes 0-6 zurück die leer(.) sind in jeder reihe


showPlayer :: Player -> String -- gibt einen string zurück 
showPlayer 1 = "X"    --Spieler 1 = X
showPlayer 2 = "O"    -- Spieler 2= O

showTile :: Tile -> Char
showTile t = if t == empty then '.' else head $ showPlayer t  -- je nach dem welcher Wert eingegeben wird 0,1,2 wird der dazupassende Char ausgegeben

showState :: State -> String
showState (State player rows) = unlines $
    map (head . show) [0 .. numCols - 1] :  --Die zahlen 0-6 werden ganz oben ausgegeben
    map (map showTile) rows                 -- Die restlichen Elemente vom Spielfeld werden richtig ausgegeben
     ++ ["\nPlayer " ++ showPlayer player ++ " to go"]



reversepc:: String -> Int 
reversepc str= (reverseTile$ head$ drop 7 $head$drop 8 $lines str) 


reverseToT :: String -> [[Tile]]
reverseToT str= map stringToTiles $ take 6(drop 1 $lines str) 


stringToTiles:: String -> [Tile]
stringToTiles str= map reverseTile str

reverseTile:: Char -> Int 
reverseTile 'X'= 1
reverseTile 'O'=2
reverseTile '.'=0

stringToState:: String -> State
stringToState str= State  (startPlayer$reversepc str) (reverseToT str)

otherPlayer :: Player -> Player  --rechnet aus welcher Spieler dran ist
otherPlayer = (3 -)

dropTile :: Move -> State -> State
dropTile col (State player rows) = State    --col ist die (Move)Spaltennummer(0-6) player(1 oder 2) und rows (der 6x7 "Array")
  (otherPlayer player)                      -- es wird ein neuer State zurückgegeben, wo der andere Spieler dran ist
  (reverse $ dropAux $ reverse rows)        --
    where
      dropAux (row : rows) =
        case splitAt col row of --es wird eine spalte und eine Reihe übergeben
         (first, t : last) ->
           if t == empty 
             then (first ++ player : last) : rows
             else row : dropAux rows

winningRow :: Player -> [Tile] -> Bool
winningRow player [] = False
winningRow player row = take 4 row == replicate 4 player
  || winningRow player (tail row)

transpose ([] : _) = []
transpose xs = map head xs : transpose (map tail xs)

winningPlayer :: State -> Maybe Player
winningPlayer (State player rows) =
  let prevPlayer = otherPlayer player
      longRows = rows ++ transpose rows++  finaldiag rows -- ++ diags rows
    in if any (winningRow prevPlayer) longRows
      then Just prevPlayer
      else Nothing

countletters:: Char  -> String -> Int

countletters a []=0
countletters a (x:xs)= if (a==x) then (1+ countletters a xs) else countletters a xs

 --diagonalen anschauen.

--diagonalen anschauen.

reversereverselist:: [[Tile]]->[[Tile]]
reversereverselist a= reverse$ map reverse a

alldiagonalsl::Int->[[Tile]]->[Tile]
alldiagonalsl sv []=[]
alldiagonalsl sv x =  (head$drop sv $ head  x) : (alldiagonalsl (sv+1) (tail  x) )


alldiagonalsfirst::Int->[[Tile]] ->[[Tile]]

alldiagonalsfirst a []=[]
alldiagonalsfirst x a= (alldiagonalsl x  a) : (alldiagonalsfirst x  (drop 1  a)) 

alldiagonalssecond::Int->[[Tile]] ->[[Tile]]
alldiagonalssecond a []=[]
alldiagonalssecond x a= alldiagonalsfirst x (reversereverselist a)
finaldiag::[[Tile]]->[[Tile]]
finaldiag  a= alldiagonalsfirst 0 a ++alldiagonalssecond 0 a ++ alldiagonalsfirst 0 (reverse a) ++alldiagonalssecond 0 (reverse a)
