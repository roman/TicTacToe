module View.NCurses 
  (
  main
  )
  where

import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import UI.HSCurses.Curses hiding (move)
import qualified UI.HSCurses.Curses as Curses
import View.Common
import Controller 
import Prelude hiding (Left, Right)

main :: (MonadIO m) => Controller m ()
main = do
  window <- liftIO $ do initCurses 
                        cursSet CursorVeryVisible
                        echo False
                        initScr
  mainMenu window
  liftIO endWin

--------------------------------------------------------------------------------
-- utility functions to avoid the us of liftIO everywhere

printOn :: (MonadIO m) => Window -> Int -> Int -> String -> Controller m ()
printOn window x y str = liftIO $ mvWAddStr window x y str >> refresh

moveCursor :: (MonadIO m) => Int -> Int -> Controller m ()
moveCursor x y = liftIO $ Curses.move x y >> refresh

refreshScreen :: (MonadIO m) => Controller m ()
refreshScreen = liftIO refresh

readKey :: (MonadIO m) => Controller m Key
readKey = liftIO getCh

--------------------------------------------------------------------------------

mainMenu :: (MonadIO m) => Window -> Controller m ()
mainMenu window = do     
  
  printOn window 0 0 "Welcome to TicTacToe"
  printOn window 2 0 "Please select:"
  printOn window 4 0 "1. Play a game"
  printOn window 5 0 "2. Exit"

  let readOption triedAlready = do
      option <- do
        when triedAlready $ printOn window 6 0 "(Invalid Option, try again)"
        printOn window 7 0 "> "
        moveCursor 7 2
        refreshScreen
        readKey
      case option of
        KeyChar '1' -> do
          player <- startGame window 
          case player of
            Just p  -> printOn window 30 0 $ "Player " ++ show p ++ " won the game"
            Nothing -> printOn window 30 0 $ "It was a tie"
          readKey
          return ()
        KeyChar '2' -> return ()
        _           -> readOption True
    
  readOption False  

drawMatrix :: (MonadIO m) => Window -> Controller m ()
drawMatrix window = do
  matrix   <- getMatrix
  position <- getPosition
  printOn window 9 0 (renderMatrix matrix)
  printOn window 16 0 ("Position: " ++ show position)
  refreshScreen

startGame :: (MonadIO m) => Window -> Controller m (Maybe Player)
startGame window = do 
  drawMatrix window
  let loop = do
      shouldStop <- processAction window
      winner     <- getWinner
      halted     <- isHalted
      case winner of
        Just player -> return $ Just player
        Nothing     -> 
          if shouldStop || halted
            then return Nothing 
            else loop

  loop

mapCursor :: (MonadIO m) => Controller m ()
mapCursor = do
  position <- getPosition
  let (x, y) = case position of
                    (1,1) -> (10, 2) 
                    (1,2) -> (10, 6)
                    (1,3) -> (10, 10)
                    (2,1) -> (11, 2) 
                    (2,2) -> (11, 6)
                    (2,3) -> (11, 10)
                    (3,1) -> (12, 2)
                    (3,2) -> (12, 6)
                    (3,3) -> (12, 10)

  moveCursor x y

processAction :: (MonadIO m) => Window -> Controller m Bool
processAction window = do
  mapCursor
  input <- readKey

  let perform movement = do 
      move movement 
      return False
      
  shouldStop <- case input of
    KeyChar 'j' -> perform Down 
    KeyChar 'k' -> perform Up  
    KeyChar 'h' -> perform Left 
    KeyChar 'l' -> perform Right 
    KeyChar ' ' -> play >> return False
    KeyChar 'q' -> return True
    _           -> return False
  printOn window 21 0 ("Last command: " ++ show input)
  drawMatrix window
  return shouldStop
    
