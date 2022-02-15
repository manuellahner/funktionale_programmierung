module Main(main) where
import Logic

main = do
  putStrLn "Welcome to Connect Four"
  game initState

game state = do
  putStrLn $ showState state
  case winningPlayer state of
    Just player -> putStrLn $ showPlayer player ++ " wins!"
    Nothing -> let moves = validMoves state in
      if null moves then putStrLn "Game ends in draw."
      else do
        putStr $ "Choose one of " ++ show moves ++ ": "
        moveStr <- getLine
        let move = (read moveStr :: Move)
        game (dropTile move state)
