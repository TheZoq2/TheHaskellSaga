module GameBoard
	( 
	generateEmptyCombatBoard,
	combatBoardToString,
	runTest
	) where


--Each turn starts out with a current board. 
--1. Each piece on that board generates all possible spaces it could move to in the next
--iteration. This can be run in parallel
--2. Each piece then processes their turn. If the desired location it wants to move to
--is not on the list of any other pieces possible movement locations then it moves itself
--there. Otherwise it generates an ordered list of movement locations where the last 
--element is guaranteed to be a non conflicted position or the piece is "destroyed"
--3. All non-conflicting mores are folded into a single list
--4. Each conflicting pieces plan is moved in order of "priority" until all pieces are 
--   without conflict

import Data.List


data Position = Position Int Int deriving(Show)


data PieceType = Empty | PlayerPiece | PawnPiece | Wall deriving(Show)

data CombatPiece = CombatPiece {
		pieceType :: PieceType,
		piecePosition :: Position,

		pieceFuture :: [CombatPiece],

		pieceGetPossibleFuture :: (CombatPiece -> CombatBoard -> [Position]),
		pieceUpdateFunction:: (CombatPiece -> CombatBoard -> ([CombatPiece], [[CombatPiece]]))
	}

data CombatBoard = CombatBoard {
		boardGrid :: [[PieceType]],
		boardPieces :: [CombatPiece]
	}



getBoardSize :: CombatBoard -> ((Int, Int), (Int, Int))
getBoardSize (CombatBoard {boardPieces = []}) = 
	((maxBound, maxBound), (minBound, minBound))
getBoardSize (CombatBoard {boardPieces = _}) =
	((0,0), (0,0))

--generateBoardGrid :: CombatBoard -> [[PieceType]]
--generateBoardGrid =
--	



--placePiece :: CombatPiece -> Position -> CombatBoard -> CombatBoard
--placePiece piece pos@(Position x y) board =
--	board { 
--		combatBoardList = 
--		runOnNth x (\lst -> runOnNth y (\_ -> piece) lst) $ combatBoardList board
--	}


runOnNth :: Int -> (a -> a) -> [a] -> [a]
runOnNth n func lst =
	runOnNthHelper 0 n func lst

runOnNthHelper :: Int -> Int -> (a -> a) -> [a] -> [a]
runOnNthHelper _ _ _ [] =
	[]
runOnNthHelper i n func (x:xs) 
	| i == n = (func $ x) : (runOnNthHelper (i+1) n func xs)
	| otherwise = x : (runOnNthHelper (i+1) n func xs)

	

----------------------------------------------------------------------------------
--							Print functions
----------------------------------------------------------------------------------

pieceToString :: PieceType -> Char
combatPieceToString piece =
	case piece of
		Empty -> '.'
		PlayerPiece -> 'P'
		PawnPiece -> 'O'
		Wall -> '#'

boardToString :: CombatBoard -> String
combatBoardToString board =
	let
		colToString col = map combatPieceToString col
	in
		concat $ intersperse "\n" $ map colToString $ combatBoardList board
	


runTest :: IO ()
runTest = do
	putStrLn $ combatBoardToString $ addWallsToBoard $ placePiece PlayerPiece (Position 5 5) $ generateEmptyCombatBoard (10,10)
