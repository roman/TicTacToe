module View 
  (
  main
  )
  where

  import Control.Monad.Trans (MonadIO)
  import Controller

  main :: (MonadIO m) => Controller m ()
  main = undefined
