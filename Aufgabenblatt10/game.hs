import System.IO

data Status = Win | Lose | Drawn | NotValid deriving (Show, Eq)
data Score = Score (Integer, Integer) deriving (Show, Eq)

start = do
  putStr "-->Game has started \n-->Player 1 VS Player 2\n\n"

screenChanger = do
  result <- readFile "changeScreen.txt"
  putStr result

isWinning :: (String, String) -> Status
isWinning (p1, p2)
 | p1 == "P" && p2 == "R" = Win
 | p1 == "R" && p2 == "S" = Win
 | p1 == "S" && p2 == "P" = Win
 | p2 == "P" && p1 == "R" = Lose
 | p2 == "R" && p1 == "S" = Lose
 | p2 == "S" && p1 == "P" = Lose
 | p1 == p2 && p1 == "P" || p1 == "S" || p1 == "R" = Drawn
 | otherwise = NotValid

addScore (Score (p1, p2)) a1 a2 = Score (p1+a1, p2+a2)

printScore s = do
    putStr ("-->" ++ show s ++"\n")

saveScore s = do
  appendFile "score.txt" (show s ++"\n")

game s = do
  saveScore s
  printScore s
  putStr "-->Player 1 - Select paper(P) scissors (S) rock (R)\n"
  p1 <- getLine
  screenChanger
  putStr "-->Player 2 - Select paper(P) scissors (S) or rock (R)\n"
  p2 <- getLine
  putStr ("-->Ergebnis: " ++ p1 ++ " VS " ++ p2++"\n")
  let
    a = isWinning (p1, p2)
    newS
      | a == Win = addScore s 1 0
      | a == Lose = addScore s 0 1
      | a == Drawn = addScore s 1 1
      | otherwise = s
  game newS

main = do
    appendFile "score.txt" "-------------------------------------------\n"
    start
    game (Score (0,0))