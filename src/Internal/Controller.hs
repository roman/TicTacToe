 module Internal.Controller 
  ( 
    Controller
  , Player (..) 
  , Movement (..)
  , evalController
  , restart
  , getPosition
  , getMatrix
  , setMatrix
  , makeMovement
  , move
  , getWinner
  , isHalted
  , play
  ) where

  import Control.Monad (liftM)
  import Control.Monad.State
  import Control.Monad.Trans
  import Data.Maybe (fromMaybe, catMaybes)
  import Data.Monoid (First(..), getFirst, mconcat)
  import Prelude hiding (Left, Right)

  import Model.Matrix 

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

  -- Implement a function that Builds a TicTacToe matrix, where each
  -- position is just Nothing (No one has made a play)
  newGameMatrix :: Matrix (Maybe Player)
  newGameMatrix = undefined

--------------------------------------------------------------------------------

  data GameState 
    = GameState {
      gamePosition  :: Position
    , gameMatrix    :: Matrix (Maybe Player)
    , currentPlayer :: Player
    }

  newtype Controller m a 
    = Controller (StateT GameState m a)
    deriving (Monad, MonadState GameState, MonadTrans, MonadIO)

  -- Roman will be discussing this function, purpose
  evalController :: (Monad m) => Controller m a -> m a
  evalController (Controller m) = evalStateT m (GameState (1,1) newGameMatrix X)
  
  -- Implement a function that gets the current State out of the Controller
  -- Monad and returns just the currentPlayer of the GameState
  getPlayer :: (Monad m) => Controller m Player
  getPlayer = undefined

  -- Implement a function that recieves an altering function, then invoked
  -- with the currentPlayer as an argument, and the result Player is stored
  -- in the GameState
  alterPlayer :: (Monad m) => (Player -> Player) -> Controller m ()
  alterPlayer = undefined
  
  -- Using alterPlayer, implement a function that changes the current player
  -- from X to O
  swapPlayer :: (Monad m) => Controller m ()
  swapPlayer = undefined
  
  -- Implement a function that gets the current State out of the Controller
  -- Monad and returns just the currentPosition of the GameState
  getPosition :: (Monad m) => Controller m Position
  getPosition = undefined

  -- Implement a function that recieves an altering function, then invoked
  -- with the currentPosition as an argument, and the result Position is stored
  -- in the GameState
  alterPosition :: (Monad m) => (Position -> Position) -> Controller m ()
  alterPosition = undefined

  -- Implement a function that returns the current matrix from the GameState
  getMatrix :: (Monad m) => Controller m (Matrix (Maybe Player))
  getMatrix = undefined

  -- Implement a function that sets the current matrix in the GameState
  setMatrix :: (Monad m) => Matrix (Maybe Player) -> Controller m ()
  setMatrix = undefined

  -- Implement a function that recieves an altering function, then invoked
  -- with the current matrix as an argument, and the result store in the
  -- GameState
  alterMatrix :: (Monad m) => (Matrix (Maybe Player) -> Matrix (Maybe Player)) -> Controller m ()
  alterMatrix = undefined

--------------------------------------------------------------------------------

  -- Optional: 
  -- Implement the increment and decrement functions, 
  -- this will increment a number and returned wrapped in a Maybe 
  -- as long as it doesn't go beyond the bounds of the TicTacToe Matrix
  -- The same idea with the decrement function
  increment, decrement :: Int -> Maybe Int
  increment = undefined
  decrement = undefined

  -- Implement a function that given a movement, will change the 
  -- current position 
  -- Hint: use the increment and decrement function
  --       use the fromMaybe function
  makeMovement :: Movement -> Position -> Position
  makeMovement = undefined

  -- Implement a function that will modify the currentPosition in the
  -- state by applying a movement to it
  -- Hint: use the alterPosition and makeMovement function to implement this
  move :: (Monad m) => Movement -> Controller m ()
  move = undefined

  -- [HARD] Implement a function that process the current game matrix on the 
  -- game state and infers if the game has a winner.
  -- Hint: use the getMatrix function
  getWinner :: (Monad m) => Controller m (Maybe Player)
  getWinner = undefined

  -- Implement a function that process the current game matrix and infers
  -- if the game is on a tie.
  isHalted :: (Monad m) => Controller m Bool
  isHalted = undefined

  -- Implement a function that will assign a player to the game matrix 
  -- in the current position of the GameState
  play :: (Monad m) => Controller m ()
  play = undefined

  -- Implement a function that will restart the matrix, and set the position
  -- to (1,1)
  restart :: (Monad m) => Controller m ()
  restart = undefined

              
