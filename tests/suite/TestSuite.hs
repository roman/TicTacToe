module Main where
  
  import           Test.Framework (defaultMain, testGroup)
  import qualified Model.Matrix.Tests
  import qualified Controller.Tests

  main :: IO ()
  main = defaultMain tests
    where tests = [
                      testGroup "Model.Matrix.Tests.tests"
                                 Model.Matrix.Tests.tests

                  ,   testGroup "Controller.Tests.tests"
                                 Controller.Tests.tests

                  ]
