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

  -- Optional:
  -- Implement a function that given a list, if empty returns nothing
  -- if not, just return the head of the list wrapped in a Maybe
  safeHead :: [a] -> Maybe a
  safeHead = undefined

  -- Optional:
  -- Implement a function that given a list, if empty returns nothing
  -- if not, just return the tail of the list wrapped in a Maybe
  safeTail :: [a] -> Maybe [a]
  safeTail = undefined

  -- Definition of the Matrix type
  -- for more info about newtype vs data:
  -- http://book.realworldhaskell.org/read/using-typeclasses.html#id608962
  newtype Matrix a  
    = Matrix [[a]] 
    deriving (Show)

  -- Given a list of rows, return a Matrix type
  buildMatrix :: [[a]] -> Matrix a
  buildMatrix = Matrix

  -- Given a Matrix, get the Rows out of it as a list of rows
  getRows :: Matrix a -> [[a]]
  getRows m = undefined

  -- Given a Matrix, get the Columns out of it as a list of columns
  getCols :: Matrix a -> [[a]]
  getCols m = undefined

  -- Given a Matrix, get the Diagonals as a list of of diagonals
  getDiagonals :: Matrix a -> [[a]]
  getDiagonals m = undefined

  -- Given a pair (Row, Col) and a Matrix return the element that is on that
  -- position wrapped in a Maybe.
  -- If the (Row,Col) combination is invalid, return Nothing
  -- Hint:
  --  * you might want to use the safeHead function here
  --  * use the Maybe Monad
  --       
  lookup :: (Int, Int) -> Matrix a -> Maybe a
  lookup = undefined

  -- Given a pair (Row, Col), a Matrix, and a function that modifies an
  -- element, return the Matrix with the element modified by the given function
  -- Hint: you might want to use the safeHead and safeTail function here
  -- 
  -- Example:
  -- 
  -- If we want to update the row 1, column 1 of a given matrix
  -- 
  --    update (1,1) (Just . (+4)) (buildMatrix [[1,1], [2,2]]) 
  --    => (True, m)
  --
  --    where
  --
  --    m == buildMatrix [[5,1], [2,2]]
  --
  -- If the index points to an invalid row or column, it should return False
  -- and the same matrix
  --
  --    update (4,4) (Just . (+4)) (buildMatrix [[1,1], [2,2]])
  --    => (False, m)
  --
  --    where
  --
  --    m == buildMatrix [[1,1], [2,2]]
  --
  -- Q: Why the modification functions returns a (Maybe a)? 
  -- A: Because we want to be able to decide in the function, if we want to update
  --    the element or not
  --
  -- Suppose we want to update the (1,1) element of the matrix, only if it is
  -- a pair number:
  --
  --    update (1,1) helper (buildMatrix [[1,1], [2,2]])
  --      where
  --        helper x = if x `mod` 2 == 0 
  --                    then (x + 1) 
  --                    else x
  --
  update :: (Int, Int) -> (a -> Maybe a) -> Matrix a -> (Bool, Matrix a)
  update = undefined
