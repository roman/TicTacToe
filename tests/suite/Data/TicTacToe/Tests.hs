module Data.TicTacToe.Tests 
  (
  tests
  ) 
  where
  

  import Control.Monad (forM)
  import Data.List (and)

  import Test.Framework (Test)
  import Test.Framework.Providers.HUnit (testCase)
  import Test.HUnit (assertBool, assertEqual)

  import Data.Matrix
  import Data.Internal.TicTacToe 

  tests  :: [Test]
  tests = [
            testPlay
          , testGetWinner
          ]

  winingGames :: [Matrix (Maybe PlayerId)]
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

  normalGame :: Matrix (Maybe PlayerId)
  normalGame = 
    buildMatrix [
      [Nothing, Just X, Nothing]
    , [Just O, Nothing, Just O]
    , [Nothing, Just X, Nothing]
    ]

            
  testPlay :: Test
  testPlay =
    testCase "TicTacToe/play" $ do
      (result, _) <- runGame $ do
                       play (1,1) O
                       a <- getWinner 
                       play (1,2) O
                       b <- getWinner 
                       play (1,3) O
                       c <- getWinner
                       return (a, b, c)
      assertEqual "TicTacToe.play is not working" (Nothing, Nothing, Just O) result

  testGetWinner :: Test
  testGetWinner =
    testCase "TicTacToe/getWinner" $ do
      (result, _) <- runGame $ do
                       forM winingGames $ \matrix -> do
                         setMatrix matrix 
                         getWinner
      assertBool "TicTacToe.getWinner is not working" $ all (== (Just X)) result

      (result', _) <- runGame $ do
                        setMatrix normalGame
                        getWinner
      assertEqual "TicTacToe.getWinner is not working" Nothing result'
    
