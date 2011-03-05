module Main where
  
  import           Test.Framework (defaultMain, testGroup)
  import qualified Data.Matrix.Tests
  import qualified Data.TicTacToe.Tests

  main :: IO ()
  main = defaultMain tests
    where tests = [
                    testGroup "Data.Matrix.Tests.tests"
                               Data.Matrix.Tests.tests
                  , testGroup "Data.TicTacToe.Tests.tests"
                               Data.TicTacToe.Tests.tests
                  ]
