module Controller 
  (
    Movement (..)
  , move
  , play
  )
  where

  import Control.Monad.State

  data Movement 
    = Right
    | Left 
    | Up 
    | Down
    deriving (Eq)

  data Player 
    = X
    | O
    deriving (Show)

  type PlayerCell = Maybe Player
  type TicTacMatrix = Matrix PlayerCell

  data ControllerState 
    = ControllerState { 
      currentPosition :: (Int, Int)
    , gameMatrix      :: TicTacMatrix
    }

  newtype Controller m a 
    = Controller (StateT ControllerState m a)
    deriving (Monad, MonadState ControllerState)

  alterController :: (MonadState m) => (ControllerState -> ControllerState) -> Controller m ()
  alterController fn = get >>= put . fn

  alterCurrentPosition :: (MonadState ControllerState m) => ((Int, Int) -> (Int, Int)) -> Controller m ()
  alterCurrentPosition fn = do
    cs <- get 
    put $ cs { currentPosition = fn (cs currentPosition) }

  increment :: Int -> Maybe Int
  increment a 
    | (a + 1) > 3 = Nothing
    | otherwise   = Just (a + 1)

  decrement :: Int -> Maybe Int
  decrement a 
    | (a - 1) < 1 = Nothing
    | otherwise   = Just (a - 1)

  updatePosition :: Movement -> (Int, Int) -> Maybe (Int, Int)
  updatePosition Down  (x, y) = increment x >>= return . (,y)
  updatePosition Up    (x, y) = decrement x >>= return . (,y) 
  updatePosition Left  (x, y) = decrement y >>= return . (x,) 
  updatePosition Right (x, y) = increment y >>= return . (x,) 

  move :: (MonadIO m) => Movement -> Controller m ()
  move movement = alterCurrentPosition . 
                  fromMaybe movement $ 
                   

  play :: (MonadIO m) => (TicTacMatrix -> IO ()) -> Controller m ()
    
  
