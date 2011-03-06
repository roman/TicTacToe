module View 
  (
  main
  )
  where

  import Control.Monad.Trans (MonadIO)
  import Controller

  import qualified View.NCurses

  main :: (MonadIO m) => Controller m ()
  main = View.NCurses.main
