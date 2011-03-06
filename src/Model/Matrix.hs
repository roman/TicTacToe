module Model.Matrix 
  (
    Matrix  
  , buildMatrix
  , getRows
  , getCols
  , getDiagonals
  , lookup
  , update
  ) where

  import           Prelude hiding (lookup)
  import           Data.List (transpose)
  import           Data.Maybe (fromMaybe)

  safeHead :: [a] -> Maybe a
  safeHead []     = Nothing
  safeHead xs = Just $ head xs

  safeTail :: [a] -> Maybe [a]
  safeTail [] = Nothing
  safeTail xs = Just $ tail xs

  newtype Matrix a 
    = Matrix { getRows :: [[a]] } deriving (Show)

  buildMatrix :: [[a]] -> Matrix a
  buildMatrix = Matrix

  getCols :: Matrix a -> [[a]]
  getCols = transpose . getRows

  getDiagonals :: Matrix a -> [[a]]
  getDiagonals m = (helper 0 $ getRows m) : (helper 0 . reverse $ getRows m) : []
    where
      helper _ [] = []
      helper n (x:xs) = head (drop n x) : helper (n + 1) xs

  lookup :: (Int, Int) -> Matrix a -> Maybe a
  lookup (x,y) m = do 
    row <- safeHead . drop (x - 1) . getRows $ m
    safeHead $ drop (y - 1) row

  update :: (Int, Int) -> (a -> Maybe a) -> Matrix a -> (Bool, Matrix a)
  update (x,y) fn matrix = 
      fromMaybe (False, matrix) $ do
        let (preR, postR) = splitAt (x - 1) (getRows matrix) 
        row <- safeHead postR 
        let (preC, postC) = splitAt (y - 1) row 
        value  <- safeHead postC
        value' <- fn value
        postR' <- safeTail postR
        postC' <- safeTail postC
        return (True, buildMatrix $ preR ++ 
                                    [preC ++ [value'] ++ postC'] ++ 
                                    postR')

