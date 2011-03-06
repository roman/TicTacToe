module Controller.Tests 
  ( 
  tests
  )
  where
  
import Prelude hiding (Left, Right)
import Control.Monad (forM)
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual, assertBool)
import Data.Matrix 
import Internal.Controller

tests :: [Test]
tests = [
          testMakeMovement
        , testMove
        , testGetWinner
        , testPlay
        ]

winingGames :: [Matrix (Maybe Player)]
winingGames =
  [
    buildMatrix [
      [Just X, Just X, Just X]             
    , [Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing]
    ]

  , buildMatrix [
      [Nothing, Nothing, Nothing]
    , [Just X, Just X, Just X]             
    , [Nothing, Nothing, Nothing]
    ]
  
  , buildMatrix [
      [Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing]
    , [Just X, Just X, Just X]             
    ]

  , buildMatrix [
      [Just X, Nothing, Nothing]             
    , [Just X, Nothing, Nothing]
    , [Just X, Nothing, Nothing]
    ]

  , buildMatrix [
      [Nothing, Just X, Nothing]             
    , [Nothing, Just X, Nothing]
    , [Nothing, Just X, Nothing]
    ]

  , buildMatrix [
      [Nothing, Nothing, Just X]             
    , [Nothing, Nothing, Just X]
    , [Nothing, Nothing, Just X]
    ]

  , buildMatrix [
      [Nothing, Nothing, Just X]             
    , [Nothing, Nothing, Just X]
    , [Nothing, Nothing, Just X]
    ]

  , buildMatrix [
      [Nothing, Nothing, Just X]             
    , [Nothing, Just X, Nothing]
    , [Just X, Nothing, Nothing]
    ]

  , buildMatrix [
      [Just X, Nothing, Nothing]
    , [Nothing, Just X, Nothing]
    , [Nothing, Nothing, Just X]             
    ]
  ]

normalGame :: Matrix (Maybe Player)
normalGame = 
  buildMatrix [
    [Nothing, Just X, Nothing]
  , [Just O, Nothing, Just O]
  , [Nothing, Just X, Nothing]
  ]

testMakeMovement :: Test
testMakeMovement = testCase "controller/makeMovement" $ do
  assertEqual "makeMovement is modifying on edges" (0, 0) $ makeMovement Left (0, 0)
  assertEqual "makeMovement is modifying on edges" (0, 0) $ makeMovement Up (0, 0)
  assertEqual "makeMovement is modifying on edges" (3, 3) $ makeMovement Right (3, 3)
  assertEqual "makeMovement is modifying on edges" (3, 3) $ makeMovement Down (3, 3)

  assertEqual "makeMovement is not working" (0,1) $ makeMovement Right (0, 0)
  assertEqual "makeMovement is not working" (1,0) $ makeMovement Down  (0, 0)
  assertEqual "makeMovement is not working" (3,2) $ makeMovement Left  (3, 3)
  assertEqual "makeMovement is not working" (2,3) $ makeMovement Up    (3, 3)


testMove :: Test
testMove = testCase "controller/move" $ do
  result <- evalController $ do
              move Right
              a <- getPosition
              move Down
              b <- getPosition
              move Left
              c <- getPosition
              move Up
              d <- getPosition
              return (a, b, c, d)
  assertEqual "move is not working" ((1,2), (2,2), (2,1), (1, 1)) result

testGetWinner :: Test
testGetWinner = testCase "controller/getWinner" $ do
  result <- evalController $ do
              forM winingGames $ \matrix -> do
                setMatrix matrix 
                getWinner
  assertBool "getWinner doesn't tell a winning game when it should" $ 
              all (== (Just X)) result

  result' <- evalController $ do
               setMatrix normalGame
               getWinner
  assertEqual "getWinner tells winner when it shouldn't" Nothing result'
    

testPlay :: Test
testPlay = testCase "controller/play" $ do
  result <- evalController $ do
              play X
              move Right
              play O
              move Down
              play X
              move Down
              play O
              move Right
              play X
              getWinner
  assertEqual "play is not working" (Just X) result

