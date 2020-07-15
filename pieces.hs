{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Pieces where

import Types
import Board

import Data.List

getRookPossible (x,y) = sortBy (closestTo (x,y)) [(i,j) | i <- [1..8], j <- [1..8], i == x || j == y]

closestTo :: Point -> Point -> Point -> Ordering
closestTo (x,y) (a,b) (c,d)
    | firstDistance < secondDistance = LT
    | firstDistance > secondDistance = GT
    | otherwise = EQ
    where
        firstDistance  = (x - a)^2 + (x - b)^2
        secondDistance = (x - c)^2 + (x - d)^2

-- pawnMoves board point = map (map $ pawnAttacksSquare board point)


-- checkForwardTiles :: Board -> Point -> Int -> MoveBoard -> MoveBoard
-- checkForwardTiles board (x,y) dir
--     | squareIsEmpty board (x,y+1) = 

-- teamInCheck

