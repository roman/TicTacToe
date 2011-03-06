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
                        initScr
  mainMenu window
  liftIO endWin

mainMenu :: (MonadIO m) => Window -> Controller m ()
mainMenu window = do     
  liftIO $ do
    mvWAddStr window 0 0 "Welcome to TicTacToe"
    mvWAddStr window 2 0 "Please select:"
    mvWAddStr window 4 0 "1. Play a game"
    mvWAddStr window 5 0 "2. Exit"

  let readOption triedAlready = do
      option <- liftIO $ do
                  when triedAlready $ mvWAddStr window 6 0 "(Invalid Option, try again)"
                  mvWAddStr window 7 0 "> "
                  Curses.move 7 2
                  refresh
                  getCh
      case option of
        KeyChar '1' -> liftIO (echo False) >> startGame window >> return ()
        KeyChar '2' -> return ()
        _           -> readOption True
    
  readOption False  

drawMatrix :: (MonadIO m) => Window -> Controller m ()
drawMatrix window = do
  matrix   <- getMatrix
  position <- getPosition
  moveCursor
  liftIO $ do 
    mvWAddStr window 9 0 (renderMatrix matrix) 
    mvWAddStr window 16 0 ("Position: " ++ show position)
    refresh

startGame :: (MonadIO m) => Window -> Controller m (Maybe Player)
startGame window = do 
  liftIO $ Curses.move 10 2
  drawMatrix window
  let loop = do
      shouldStop <- processAction window
      winner <- getWinner
      case winner of
        Just player -> return $ Just player
        Nothing     -> if shouldStop then return Nothing else loop

  loop

moveCursor :: (MonadIO m) => Controller m ()
moveCursor = do
  position <- getPosition
  liftIO $ do
    case position of
      (1,1) -> Curses.move 10 2
      (1,2) -> Curses.move 10 5
      (1,3) -> Curses.move 10 8
      (2,1) -> Curses.move 12 2 
      (2,2) -> Curses.move 12 5
      (2,3) -> Curses.move 12 8
      (3,1) -> Curses.move 14 2
      (3,2) -> Curses.move 14 5
      (3,3) -> Curses.move 14 8
    refresh

processAction :: (MonadIO m) => Window -> Controller m Bool
processAction window = do
  input <- liftIO getCh 
  shouldStop <- case input of
    KeyChar 'j'    -> move Down  >> return False
    KeyChar 'k'    -> move Up    >> return False 
    KeyChar 'h'    -> move Left  >> return False
    KeyChar 'l'    -> move Right >> return False
    KeyChar '\ESC' -> return True
    _              -> return False
  liftIO $ mvWAddStr window 21 0 ("Last command: " ++ show input)
  drawMatrix window
  return shouldStop
    
