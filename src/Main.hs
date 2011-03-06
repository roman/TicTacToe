module Main where
  
  import Data.TicTacToe (evalGame)
  import qualified View.NCurses as Application 

  main :: IO ()
  main = evalGame Application.main
