module Pieces where

import Types
import Board

import Data.List
import Data.Array

getRookPossible :: Point -> [Point]
getRookPossible (x,y) = sortBy (closestTo (x,y)) [(i,j) | i <- [1..8], j <- [1..8], i == x || j == y]
getRookActual :: Board -> Point -> [Point] -> [Point] -> [Point]
getRookActual board from acc (x:xs)
    | squareIsEmpty board (getCloserPointTo from x) = getRookActual board from (x : acc) xs
    | otherwise = getRookActual board from acc xs
getRookActual board from acc [] = acc

getRookActual' = getRookActual initialBoard (3,3) [] (getRookPossible (3,3))

getCloserPointTo :: Point -> Point -> Point
getCloserPointTo (x,y) (a,b)
    | x == a && y == b = (x,y)
    | x == a = (a, b + unit y b)
    | y == b = (a + unit x a, b)
    | otherwise = error "Called in non-rectangular manner"

unit :: Int -> Int -> Int
unit x y = round $ fromIntegral (x - y) / (fromIntegral $ abs (x - y))

closestTo :: Point -> Point -> Point -> Ordering
closestTo (x,y) (a,b) (c,d)
    | firstDistance < secondDistance = LT
    | firstDistance > secondDistance = GT
    | otherwise = EQ
    where
        firstDistance  = (x - a)^2 + (x - b)^2
        secondDistance = (x - c)^2 + (x - d)^2

equalDistance :: Point -> Point -> Point -> Bool
equalDistance p1 p2 p3 = closestTo p1 p2 p3 == EQ
