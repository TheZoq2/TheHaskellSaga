module GameBoard
    ( 
	generateEmptyCombatBoard,
	combatBoardToString,
	runTest
    ) where

import Data.List


data Position = Position Int Int deriving(Show)

data Pawn = Pawn {
		pawnPosition :: Position
	} deriving(Show)

data CombatPiece = Empty | PlayerPiece | PawnPiece | Wall deriving(Show)

data CombatBoard = CombatBoard {
		combatBoardList :: [[CombatPiece]],
		combatBoardSize :: (Int, Int)
	} deriving(Show)


generateEmptyCombatBoard :: (Int, Int) -> CombatBoard
generateEmptyCombatBoard size@(width, height) =
	let
		emptyList = replicate width (replicate height Empty)
	in
		CombatBoard {combatBoardList = emptyList, combatBoardSize = size}




populateCombatBoard :: CombatBoard -> [(CombatBoard -> CombatBoard)] -> CombatBoard
populateCombatBoard original [] =
	original
populateCombatBoard original (func:nextFuncs) =
	let
		nextBoard = populateCombatBoard original nextFuncs
	in
		func nextBoard


addWallsToBoard :: CombatBoard -> CombatBoard
addWallsToBoard board =
	let
		size@(_, height) = combatBoardSize board
		oldList = combatBoardList board

		addWallsToSides originalRow = 
			[Wall]
			++
			(init $ tail originalRow)
			++
			[Wall]

		fullCol = replicate height Wall

		newList = 
			[fullCol]
			++
			(map addWallsToSides $ init $ tail $ oldList)
			++
			[fullCol]
	in
		CombatBoard {combatBoardList = newList, combatBoardSize = size}


placePiece :: CombatPiece -> Position -> CombatBoard -> CombatBoard
placePiece piece pos@(Position x y) board =
	board { 
		combatBoardList = 
		runOnNth x (\lst -> runOnNth y (\_ -> piece) lst) $ combatBoardList board
	}


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

combatPieceToString :: CombatPiece -> Char
combatPieceToString piece =
	case piece of
		Empty -> '.'
		PlayerPiece -> 'P'
		PawnPiece -> 'O'
		Wall -> '#'

combatBoardToString :: CombatBoard -> String
combatBoardToString board =
	let
		colToString col = map combatPieceToString col
	in
		concat $ intersperse "\n" $ map colToString $ combatBoardList board
	


runTest :: IO ()
runTest = do
	putStrLn $ combatBoardToString $ addWallsToBoard $ placePiece PlayerPiece (Position 5 5) $ generateEmptyCombatBoard (10,10)
