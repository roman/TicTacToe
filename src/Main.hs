module Main where
  
  import Controller (evalController)
  import qualified View as View

  main :: IO ()
  main = evalController View.main
