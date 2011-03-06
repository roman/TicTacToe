module View.NCurses 
  (
  main
  )
  where
  
  import Control.Monad.Trans (MonadIO, liftIO)
  import System.IO (stdout, hFlush)
  import UI.HSCurses.Curses

  import Data.TicTacToe

  main :: (MonadIO m) => TicTacToe m ()
  main = do 
    window <- liftIO $ do
                initCurses
                cursSet CursorVisible
                initScr 
    option <- menu window
    case option of
      '1' -> return ()
      '2' -> return ()
    liftIO endWin
    liftIO $ putStrLn "Exiting Game"

  menu :: (MonadIO m) => Window -> TicTacToe m Char
  menu window = liftIO $ do
    let inputLoop = do {
      result <- getCh;
      case result of
        KeyChar n -> return n;
        _         -> inputLoop;
    }
    mvWAddStr window 0 0 "Welcome to TicTacToe"
    mvWAddStr window 1 0 "Choose an option:"
    mvWAddStr window 2 0 "1. Start Game"
    mvWAddStr window 3 0 "2. Exit Game"
    mvWAddStr window 4 0 "> "
    refresh
    inputLoop

  --renderGame :: (MonadIO m) => TicTacToe m ()
  --renderGame = do
  --  liftIO $ do
  --    initCurses
  --    window <- initScr
  --    echo False
  --    keypad window True
  --    mvWAddStr window 0 0 "Welcome to TicTacToe"
  --    refresh
  --    endWin

  


