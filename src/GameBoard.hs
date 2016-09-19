module GameBoard
    ( 
    ) where


data Position = Float Float

data Pawn = Pawn {
		pawnPosition :: Position
	}

data CombatPiece = Empty | PlayerPiece | PawnPiece | Wall deriving(Show)

data CombatBoard = CombatBoard [[CombatPiece]]


generateCombatBoard :: (Int, Int) -> CombatBoard
generateCombatBoard (width, height) =
	let
		emptyBoard = CombatBoard $ take width $ (take height $ (Empty..))
	in
		emptyBoard
