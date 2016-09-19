module Lib
    ( someFunc,
	getRandomNrInRange
    ) where

import System.Random
import Data.List

data DamageType = DamageType Float Float Float deriving(Show)


data Weapon = Weapon {
			weaponName :: String,
			weaponDamage :: DamageType
			} deriving(Show)

data DefenceType = DefenceType Float Float Float deriving(Show)
data MonsterAttackType = MonsterAttackType Float Float Float deriving(Show)

data Monster = Monster
			{
				monsterName :: String,
				monsterDefence :: DefenceType,
				monsterAttack :: MonsterAttackType
			}

getNRandomElements :: [a] -> Int -> [Int] -> ([a], [Int])
getNRandomElements vals n randList =
	let
		rands = take n randList
	in
		(map (\index -> vals !! mod index (length vals)) rands, drop n randList)


getBoundedRandomNr :: [Int] -> Int-> (Int, [Int])
getBoundedRandomNr randList max =
	(mod (head randList) max, tail randList)

getRandomNrInRange :: [Int] -> (Int,  Int) -> (Int, [Int])
getRandomNrInRange randList (minVal, maxVal) =
	let
		randBounds = maxVal - minVal + 1

		(boundedNr, randList') = (getBoundedRandomNr randList randBounds)
	in
		((minVal + boundedNr), randList')




generateGenericThing :: 
		[
			(
				(Int, Int), --The amount of this to choose
				[(String, [Float])],  --The name along with a list of modifiers
				Float -> Float -> Float --Function for "adding" the modifiers in this
									 --step
			)
		] -> 
		Int -> --The amount of parameters to generate
		[Int] -> 
		(String, [Float], [Int]) --Tuple with the thing name, its parameters 
				--and the new randList
generateGenericThing [] paramAmount randList = --Base case
	("", replicate paramAmount 0, randList)
generateGenericThing parameters paramAmount randList =
	let

		(nextName, nextParams, _) = 
				generateGenericThing (tail parameters) paramAmount (drop 25 randList)

		(amountRange,paramChoises,addFunction) = head parameters

		(amountToChose, randList2) = getRandomNrInRange randList amountRange

		--Selecting amountTochose parameters 
		(params, randList3) = getNRandomElements paramChoises amountToChose randList2

		thisName = concat $ map (\str -> str ++ " ") $ map fst params

		finalParams = foldr (zipWith addFunction) nextParams $ map snd params
	in
		(thisName ++ nextName, finalParams, randList)


generateMonster :: [Int] -> (String, [Float], [Int])
generateMonster randList =
	let
		options = [
				(
					(1, 3),
					[
						("cute", 		[1.1, 1.2, 1.3]),
						("dangerous", 	[1.1, 1.2, 1.3]),
						("hungry", 		[1.1, 1.2, 1.3]),
						("lazy", 		[1.1, 1.2, 1.3]),
						("sneaky", 		[1.1, 1.2, 1.3]),
						("evil", 		[1.1, 1.2, 1.3]),
						("friendly", 	[1.1, 1.2, 1.3]),
						("small", 		[1.1, 1.2, 1.3]),
						("big", 		[1.1, 1.2, 1.3])
					],
					(*)
				),
				(
					(1,1),
					[
						("list", 				[1,1,1]),
						("monad", 				[1,1,1]),
						("tuple", 				[1,1,1]),
						("infinite list", 		[1,1,1]),
						("syntax error", 		[1,1,1]),
						("lambda", 				[1,1,1]),
						("maybe", 				[1,1,1]),
						("either", 				[1,1,1]),
						("functor", 			[1,1,1]),
						("applicative functor", [1,1,1])
					],
					(+)
				)
			]

		paramAmount = 3
	in
		generateGenericThing options paramAmount randList


printMonster :: (String, [Float], [Int]) -> String
printMonster (name, params, _) =
	name ++ "[" ++ (concat $ intersperse ", " $ map show params) ++ "]"

someFunc :: IO ()
someFunc = do
	r1 <- getStdGen
	let randList  = randomRs (0, maxBound) r1 :: [Int]
	putStrLn $ show $ fst $ generateEditor randList




generateEditor :: [Int] -> (Weapon, [Int])
generateEditor randList =
	let
		options = [
				(
					(1, 3),
					[
						("uncompiled", 				[0.5, 1.2, 0.5]),
						("outdated", 				[1.3, 0.4, 1.3]),
						("microsoft™", 				[0.4, 0.1, 0.3]),
						("customized", 				[1.5, 0.7, 1.0]),
						("russian", 				[0.6, 1.0, 0.8]),
						("obfuscated", 				[0.2, 1.1, 1.0]),
						("encrypted", 				[0.9, 2.0, 1.0]),
						("standard", 				[1.0, 1.0, 1.0]),
						("beta", 					[0.8, 0.8, 0.7]),
						("alpha", 					[0.5, 0.5, 0.5]),
						("pre alpha", 				[0.3, 0.3, 0.3]),
						("deprecated", 				[1.2, 0.8, 1.1]),
						("optimized", 				[1.1, 1.0, 1.0]),
						("brand new", 				[0.7, 1.2, 1.3]),
						("latest version of", 		[1.1, 1.2, 1.3]),
						("limited edition", 		[1.0, 1.0, 1.0]),
						("cloud connected", 		[0.8, 1.5, 1.3]),
						("facebook integrated", 	[0.0, 0.0, 0.0]),
						("buggy", 					[0.7, 0.5, 0.8]),
						("javascript framework for",[0.0, 0.0, 0.0])
					],
					(*)
				),
				(
					(1,1),
					[
						("vim", 				[3.00, 1.00, 3.00]),
						("emacs", 				[0.50, 1.00, 2.00]),
						("nano", 				[0.50, 1.00, 0.50]),
						("notepad", 			[1.00, 1.00, 0.20]),
						("gedit", 				[1.50, 1.00, 1.00]),
						("sublime text", 		[1.80, 1.00, 1.10]),
						("magnet", 				[0.05, 0.20, 1.00]),
						("butterfly", 			[0.00, 0.00, 0.00]),
						("car battery", 		[0.01, 0.02, 0.00]),
						("wordpad", 			[1.00, 1.00, 0.00]),
						("microsoft word", 		[1.20, 1.00, 0.10]),
						("vi", 					[2.00, 1.00, 2.00]),
						("neovim", 				[3.10, 1.00, 3.10]),
						("screwdriver", 		[0.10, 1.00, 0.00]),
						("cosmic rays", 		[0.00, 0.00, -1.0]),
						("javascript framework",[1.50, 0.30, 1.70])
					],
					(+)
				),
				(
					--[usability, security, usefullness]
					(0,1),
					[
						("on gentoo", 					[0.70, 0.70, 3.00]),
						("on windows™", 				[-2.0, -2.0, -2.0]),
						("on mac™", 					[1.00, 1.30, 0.30]),
						("on iOS™", 					[1.10, 1.30, 0.00]),
						("on linux", 					[1.50, 1.50, 1.60]),
						("on temple OS",				[0.50, 3.00, 0.30]),
						("on ubuntu", 					[1.40, 0.90, 0.90]),
						("on a raspberry pi", 			[0.70, 1.70, 0.70]),
						("on arduino", 					[0.20, 3.00, 0.20]),
						("on CD-ROM", 					[0.00, 0.00, 0.00]),
						("on paper", 					[-100, 6.00, -100]),
						("on floppy disk", 				[0.00, 2.00, 0.00]),
						("on a javascript framework",	[1.50, 0.30, 1.60])
					],
					(+)
				)
			]

		paramAmount = 3

		(generatedName, (dmg1:dmg2:dmg3:_), newRandList) = generateGenericThing options paramAmount randList

		damage = DamageType dmg1 dmg2 dmg3
	in
		(Weapon {weaponName = generatedName, weaponDamage = damage}, newRandList)
