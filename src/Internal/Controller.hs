 module Internal.Controller 
  ( 
    Player (..) 
  , Movement (..)
  , evalController
  , getPosition
  , getMatrix
  , setMatrix
  , makeMovement
  , move
  , getWinner
  , play
  ) where

  import Control.Monad (liftM)
  import Control.Monad.State
  import Control.Monad.Trans
  import Data.Matrix 
  import Data.Maybe (fromMaybe, catMaybes)
  import Data.Monoid (First(..), getFirst, mconcat)
  import Prelude hiding (Left, Right)

  type Position = (Int, Int)

  data Player
    = X
    | O
    deriving (Show, Eq)

  data Movement 
    = Up
    | Down
    | Left
    | Right
    deriving (Eq)

  newGameMatrix :: Matrix (Maybe Player)
  newGameMatrix = 
      buildMatrix  [
        replicate 3 Nothing
      , replicate 3 Nothing
      , replicate 3 Nothing 
      ]

--------------------------------------------------------------------------------

  data GameState 
    = GameState {
      gamePosition :: Position
    , gameMatrix   :: Matrix (Maybe Player)
    }

  newtype Controller m a 
    = Controller (StateT GameState m a)
    deriving (Monad, MonadState GameState, MonadTrans, MonadIO)

  evalController :: (Monad m) => Controller m a -> m a
  evalController (Controller m) = evalStateT m (GameState (1,1) newGameMatrix)

  
  getPosition :: (MonadState GameState m) => m Position
  getPosition = gamePosition `liftM` get

  alterPosition :: (MonadState GameState m) => (Position -> Position) -> m ()
  alterPosition fn = do
    gs <- get
    put $ gs { gamePosition = fn (gamePosition gs) }

  getMatrix :: (MonadState GameState m) => m (Matrix (Maybe Player))
  getMatrix = gameMatrix `liftM` get

  setMatrix :: (MonadState GameState m) => Matrix (Maybe Player) -> m ()
  setMatrix matrix = get >>= \gs -> put $ gs { gameMatrix = matrix }

  alterMatrix :: (MonadState GameState m) => (Matrix (Maybe Player) -> Matrix (Maybe Player)) -> m ()
  alterMatrix fn = do
    gs <- get
    put $ gs { gameMatrix = fn (gameMatrix gs) }

--------------------------------------------------------------------------------

  increment, decrement :: Int -> Maybe Int
  increment a = if a >= 3 then Nothing else Just (a + 1)
  decrement a = if a <= 1 then Nothing else Just (a - 1)

  makeMovement :: Movement -> Position -> Position
  makeMovement Up    p@(x, y) = fromMaybe p (decrement x >>= return . (,y))
  makeMovement Down  p@(x, y) = fromMaybe p (increment x >>= return . (,y))
  makeMovement Left  p@(x, y) = fromMaybe p (decrement y >>= return . (x,))
  makeMovement Right p@(x, y) = fromMaybe p (increment y >>= return . (x,))

  move :: (MonadState GameState m) => Movement -> m ()
  move movement = alterPosition (makeMovement movement)

  getWinner :: (MonadState GameState m) => m (Maybe Player)
  getWinner = get >>= return . checkMatrix . gameMatrix
    where
      checkMatrix :: Matrix (Maybe Player) -> Maybe Player
      checkMatrix matrix = 
          getFirst .
          mconcat  . 
          map (findWinner True) .
          catMaybes . 
          map sequence $ 
            getRows matrix ++ 
            getCols matrix ++
            getDiagonals matrix

      findWinner :: Bool -> [Player] -> First Player
      findWinner False _  = First Nothing
      findWinner True [x] = First $ Just x
      findWinner True (x:y:xs) = findWinner (x == y) (y:xs)

  play :: (MonadState GameState m) => Player -> m ()
  play player = do
    position <- getPosition 
    alterMatrix (snd . update position (Just . Just . fromMaybe player))
              
