import Data.List
import Data.Char

data Team = Black | White

data Piece = Pawn Team
    | Rook Team
    | Knight Team
    | Bishop Team
    | King Team
    | Queen Team
    | None

instance Show Piece where
    show (Pawn t) = caseFromTeam t "p"
    show (Rook t) = caseFromTeam t "r"
    show (Knight t) = caseFromTeam t "n"
    show (Bishop t) = caseFromTeam t "b"
    show (King t) = caseFromTeam t "k"
    show (Queen t) = caseFromTeam t "q"
    show _ = "0"

type Board = [[Piece]]
type Point = (Int, Int)
    
caseFromTeam White = map toUpper
caseFromTeam Black = map toLower


teamlessBackRow = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
teamlessPawnRow = replicate 8 Pawn

initialBoard = transpose $ [map ($ White) teamlessBackRow, map ($ White) teamlessPawnRow]
    ++ (replicate 4 $ replicate 8 (None))
    ++ [map ($ Black) teamlessPawnRow, map ($ Black) teamlessBackRow]

showBoard :: Board -> String
showBoard = unlines . map (intercalate " ") . map (map show)

codeAt :: Board -> Point -> Piece
codeAt board (x, y) = board !! x !! y

-- squareIsPawn :: Board -> (Num, Num)
squareIsPawn board p = case (codeAt board p) of
    Pawn _ -> True
    _ -> False

-- teamAtIs :: Board -> Point -> Team -> Bool
-- teamAtIs board p team = case ((codeAt board p) t) of
--     White -> True
--     Black -> False

teamIs :: (Piece a) => a -> Bool
teamIs (Pawn a) = a

main = do 
    -- putStrLn "running"
    -- putStrLn $ show (cmap initialBoard)

    putStrLn $ show $ codeAt initialBoard (1, 1)
    print $ squareIsPawn initialBoard (1, 6)
    -- print $ teamAtIs initialBoard (1, 1) Black
    print $ teamIs Pawn Black

-- codeAt board 