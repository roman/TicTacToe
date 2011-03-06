module View.Common 
  (
  renderMatrix
  )
  where

  import Data.List (replicate, intercalate)
  import Model.Matrix
  import Controller (Player(..))
  
  renderPlayer :: Maybe Player -> String
  renderPlayer Nothing  = "   "
  renderPlayer (Just p) = " " ++ show p ++ " "
  
  surround :: [a] -> [[a]] -> [[a]]
  surround a [] = [a]
  surround a (x:xs) = (a : x : surround a (xs))

  renderMatrix :: Matrix (Maybe Player) -> String
  renderMatrix = 
      intercalate "\n"             .
      (++ ["+-----------+"])       .
      (["+-----------+"] ++)       .
      map (concat . surround "|")  .
      map (map renderPlayer)       .
      getRows 

