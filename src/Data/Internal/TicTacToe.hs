module Data.Internal.TicTacToe 
  (
    TicTacToe 
  , PlayerId (..)
  , runGame 
  , setMatrix
  , play
  , getWinner
  ) where

  import Control.Monad (when, sequence)
  import Control.Monad.State (MonadState, get, put, StateT, runStateT)
  import Control.Monad.Trans (MonadIO, MonadTrans)
  import Data.Matrix (Matrix, buildMatrix, getRows, getCols, getDiagonals, update)
  import Data.Maybe (catMaybes)
  import Data.Monoid (First(..), getFirst, mconcat)

  newtype TicTacToe m a 
    = TTT (StateT GameInfo m a) 
    deriving (Monad, MonadState GameInfo, MonadTrans, MonadIO)

  data PlayerId
    = X 
    | O
    deriving (Eq, Show)

  data GameInfo
    = GameInfo {
      gameMatrix :: Matrix (Maybe PlayerId)
    } deriving (Show)

  newGameMatrix :: Matrix (Maybe PlayerId)
  newGameMatrix = 
      buildMatrix  [
        replicate 3 Nothing
      , replicate 3 Nothing
      , replicate 3 Nothing 
      ]


  runGame :: (Monad m) => TicTacToe m a -> m (a, GameInfo)
  runGame (TTT m) = runStateT m (GameInfo newGameMatrix)

  updateGameInfo :: (MonadState GameInfo m) => (GameInfo -> GameInfo) -> m ()
  updateGameInfo fn = get >>= put . fn

  setMatrix :: (MonadState GameInfo m) => Matrix (Maybe PlayerId) -> m ()
  setMatrix matrix = updateGameInfo (\gi -> gi { gameMatrix = matrix })

  play :: (MonadState GameInfo m) => (Int,Int) -> PlayerId -> m Bool
  play key value = do
    gameInfo <- get
    let (b, matrix') = update key (const $ Just (Just value)) (gameMatrix gameInfo)
    when b (put $ gameInfo { gameMatrix = matrix' })
    return b

  getWinner :: (MonadState GameInfo m) => m (Maybe PlayerId)
  getWinner = get >>= return . checkMatrix . gameMatrix
    where
      checkMatrix :: Matrix (Maybe PlayerId) -> Maybe PlayerId
      checkMatrix matrix = 
          getFirst .
          mconcat . 
          map (findWinner True) .
          catMaybes . 
          map sequence $ 
            getRows matrix ++ 
            getCols matrix ++
            getDiagonals matrix

      findWinner :: Bool -> [PlayerId] -> First PlayerId
      findWinner False _  = First Nothing
      findWinner True [x] = First $ Just x
      findWinner True (x:y:xs) = findWinner (x == y) (y:xs)


