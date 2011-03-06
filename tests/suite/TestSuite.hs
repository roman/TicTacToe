module Main where
  
  import           Test.Framework (defaultMain, testGroup)
  import qualified Data.Matrix.Tests
  import qualified Controller.Tests

  main :: IO ()
  main = defaultMain tests
    where tests = [
                      testGroup "Data.Matrix.Tests.tests"
                                 Data.Matrix.Tests.tests

                  ,   testGroup "Controller.Tests.tests"
                                 Controller.Tests.tests

                  ]
