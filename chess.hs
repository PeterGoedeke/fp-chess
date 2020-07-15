{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import Types
import Board
import Pieces
import Data.Array

main = do
    print $ kingFind initialBoard White