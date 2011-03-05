module Data.Matrix.Tests 
  (
  tests
  ) 
  where

  import           Prelude hiding (lookup)
  import           Test.Framework (Test)
  import           Test.Framework.Providers.HUnit (testCase)
  import           Test.HUnit (assertEqual, assertBool)
  import           Data.Matrix 
  
  tests :: [Test]
  tests = [
            testGetRows 
          , testGetCols
          , testGetDiagonals
          , testLookup
          , testUpdate
          ]

  testGetRows :: Test
  testGetRows = testCase "matrix/getRows" $ do
    let matrix = buildMatrix [ [1,2,3]
                             , [4,5,6]
                             , [7,8,9]
                             ]
    assertEqual "getRows is not working" 
                [[1,2,3], [4,5,6], [7,8,9]]
                (getRows matrix)

  testGetCols :: Test
  testGetCols = testCase "matrix/getCols" $ do
    let matrix = buildMatrix [ [1,2,3]
                             , [4,5,6]
                             , [7,8,9]
                             ]
    assertEqual "getCols is not working"
                [[1,4,7], [2,5,8], [3,6,9]]
                (getCols matrix)

  testGetDiagonals :: Test
  testGetDiagonals = testCase "matrix/getDiagonals" $ do
    let matrix = buildMatrix [ [1,2,3]
                             , [4,5,6]
                             , [7,8,9]
                             ]
    assertEqual "getDiagonals is not working"
                [[1,5,9], [7,5,3]]
                (getDiagonals matrix)

  testLookup :: Test
  testLookup = testCase "matrix/lookup" $ do
    let matrix = buildMatrix [ [1,2,3]
                             , [4,5,6]
                             , [7,8,9]
                             ]
    assertEqual "lookup is not working"
                 (Just 6)
                 (lookup (2,3) matrix)

  testUpdate :: Test 
  testUpdate = testCase "matrix/update" $ do
    let matrix = buildMatrix [ [1,2,3]
                             , [4,5,6]
                             , [7,8,9]
                             ]
    assertBool "update returns true when index is valid" 
               (fst $ update (1,3) (const $ Just 4) matrix)

    assertEqual "update modifies the matrix when index is valid" 
                [[1,2,4], [4,5,6], [7,8,9]]
                (getRows . snd $ update (1,3) (const $ Just 4) matrix)

    assertBool "update returns false when index is invalid"
               (not . fst $ update (4,5) (const $ Just 4) matrix)

    assertEqual "update doesn't modify the matrix when index is invalid"
                [[1,2,3], [4,5,6], [7,8,9]]
                (getRows . snd $ update (4,5) (const $ Just 4) matrix)
                

                

