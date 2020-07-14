{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import Data.List
import Data.Char
import Control.Bool

data Team = Black | White deriving (Show, Eq)

data PieceType = Pawn
    | Rook
    | Knight
    | Bishop
    | King
    | Queen

data PieceOf = Piece PieceType Team | None

instance Show PieceOf where
    show (Piece Pawn t) = caseFromTeam t "p"
    show (Piece Rook t) = caseFromTeam t "r"
    show (Piece Knight t) = caseFromTeam t "n"
    show (Piece Bishop t) = caseFromTeam t "b"
    show (Piece King t) = caseFromTeam t "k"
    show (Piece Queen t) = caseFromTeam t "q"
    show None = "0"

type Board = [[PieceOf]]
type Point = (Int, Int)
    
caseFromTeam White = map toUpper
caseFromTeam Black = map toLower


teamlessBackRow = [Piece Rook, Piece Knight, Piece Bishop, Piece Queen, Piece King, Piece Bishop, Piece Knight, Piece Rook]
teamlessPawnRow = replicate 8 (Piece Pawn)

initialBoard = transpose $ [map ($ White) teamlessBackRow, map ($ White) teamlessPawnRow]
    ++ (replicate 4 $ replicate 8 (None))
    ++ [map ($ Black) teamlessPawnRow, map ($ Black) teamlessBackRow]

showBoard :: Board -> String
-- merge the two maps
showBoard = unlines . map (intercalate " ") . map (map show)

codeAt :: Board -> Point -> PieceOf
codeAt board (x, y) = board !! x !! y

-- squareIsPawn :: Board -> (Num, Num)
squareIsPawn board point = case (codeAt board point) of
    Piece Pawn _ -> True
    _ -> False

-- teamAtIs :: Board -> Point -> Team -> Bool
-- teamAtIs board p team = case ((codeAt board p) t) of
--     White -> True
--     Black -> False

-- teamIs :: PieceOf -> Maybe a
teamIs :: PieceOf -> Team -> Maybe Bool
teamIs (Piece _ t) team = Just $ t == team
teamIs (None) team = Nothing

teamIsnt :: PieceOf -> Team -> Maybe Bool
teamIsnt = (notM .) . teamIs

main = do 
    putStrLn $ showBoard initialBoard