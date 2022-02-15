module Main(main) where

import Logic

import System.IO
import Text.Read (Lexeme(String))

a::Int  
a=1


main = do
  
  hSetBuffering stdout NoBuffering    -- avoid buffering problems
  putStrLn "Welcome to Connect Four"
  putStrLn "(n)ew game or (l)oad game:"
  mychoice <- getLine 
  if mychoice==n then game (initState $startPlayer 1) a else loadgame

game:: State -> Int -> IO()
game state pc = do
  
  putStrLn $ showState state 
  case winningPlayer state of
    Just player ->do 
      putStrLn $ showPlayer player ++ " wins!"
      newgame pc
    Nothing -> let moves = validMoves state in
      if null moves then putStrLn "Game ends in draw."
      else do
        putStr $ "Choose one of " ++ show moves ++ ": "
       
        myfun state pc

        
       
myfun state st = do
  moveStr <- getLine
  
  
  if (moveStr=="0" || moveStr=="1" || moveStr=="2" || moveStr=="3" || moveStr=="4" || moveStr=="5" || moveStr=="6") then do
    putStrLn "...accept and continue..."
    let move = (read moveStr :: Move) in              
     game (dropTile move state) st else 
       do
        putStrLn (moveStr ++ " is not a valid input, try again:")
        myfun state st

newgame:: Int -> IO()
newgame st  = do
  putStrLn "do you want to play an other game? y(es) n(o)"
  moveStr <- getLine
  if(moveStr=="y") then do
    game (initState $startPlayer((st `mod` 2)+1)) ((st `mod` 2)+1)
    
  else 
    do
      putStr"Crtl+C zum beenden drücken"


loadgame=