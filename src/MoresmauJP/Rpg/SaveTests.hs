-- | Saving and loading hunit tests
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.SaveTests where

import Control.Monad

import qualified Data.Map as M

import MoresmauJP.Maze1.Maze 
import MoresmauJP.Rpg.Character
import MoresmauJP.Rpg.MazeObjects
import MoresmauJP.Rpg.Save
import MoresmauJP.Rpg.CharacterTests
import MoresmauJP.Util.Random

import System.Directory
import System.FilePath
import System.Random
import Test.HUnit

saveTests = TestList [testNormalCharacterFlow,testNormalGameFlow,
	testBackupGameFlow,testBackupAndCurrent,testSaveCurrentOOS]

testNormalCharacterFlow = TestLabel "testNormalCharacterFlow" (TestCase (do
	rpg<-getTestRpg
	let name="JP"
	let c=createTestChar name
	e<-saveCharacter rpg c
	assertBool "e is not Right" (case e of 
		(Right _)-> True
		_ -> False)
	characterExists<-doesCharacterExists rpg name
	assertBool "character does not exist" characterExists
	currentGameExists<-doesCurrentGameExists rpg name
	assertBool "game does exist" (not currentGameExists)
	chars<-listCharacters rpg
	assertEqual "not 1 character" 1 (length chars)
	assertEqual "not JP" name (head chars) 
	games<-listGames rpg name
	assertEqual ("game found "++show (head games)) 0 (length games)
	ed<-deleteCharacter rpg name
	assertBool "ed is not Right" (case ed of 
		(Right _)-> True
		_ -> False)
	characterExists2<-doesCharacterExists rpg name
	assertBool "character still exists" (not characterExists2)
	currentGameExists2<-doesCurrentGameExists rpg name
	assertBool "game does exist" (not currentGameExists2)
	chars2<-listCharacters rpg
	assertEqual "not 1 character" 0 (length chars2)
	))
	
testNormalGameFlow = TestLabel "testNormalGameFlow" (TestCase (do
	rpg<-getTestRpg
	let name="JP"
	let c=createTestChar name
	saveCharacter rpg c
	sg<-getStdGen
	gw<-evalRandT (generateGameWorld (10,5)) (ProductionRandom sg)
	let mgs=RPGGameState (gw) (MazeObjects M.empty M.empty) 0
	let rpg2=rpg{rpgCharacter=Just c,mgs=Just mgs}
	e<-saveCurrent rpg2
	assertBool ("e is not Right "++(show e)) (case e of 
		(Right _)-> True
		_ -> False)
	games<-listGames rpg name
	assertEqual "not 1 game found" 1 (length games)
	assertBool "head hames is not Right" (case (head games) of 
		(Right _)-> True
		_ -> False)
	let (Right (n,_))=head games
	assertEqual "not name" name n
	
	lb<-loadBackup rpg name
	assertBool "lb is not Right" (case (lb) of 
		(Right _)-> True
		_ -> False)
	
	eD<-deleteGame rpg2 name
	assertBool ("eD is not Right: "++(show eD)) (case eD of 
		(Right _)-> True
		_ -> False)
	currentGameExists<-doesCurrentGameExists rpg name
	assertBool "game does exist" (not currentGameExists)
	chars<-listCharacters rpg
	assertEqual "not 1 character" 1 (length chars)
	assertEqual "not JP" name (head chars) 
	games<-listGames rpg name
	assertEqual ("game found "++show (head games)) 0 (length games)
	
	lb2<-loadBackup rpg name
	assertBool "lb2 is not Left" (case lb2 of 
		(Right _)-> False
		(Left _) -> True)
	
	
	))
	
testBackupGameFlow = TestLabel "testBackupGameFlow" (TestCase (do
	tmp<-getTemporaryDirectory
	let dir=combine tmp "rpg"
	removeDirectoryRecursive dir
	let rpg=RPGState Nothing Nothing dir
	let name="JP"
	let c=createTestChar name
	saveCharacter rpg c
	sg<-getStdGen
	gw<-evalRandT (generateGameWorld (10,5)) (ProductionRandom sg)
	let mgs=RPGGameState (gw) (MazeObjects M.empty M.empty) 0
	let rpg2=RPGState (Just c) (Just mgs) dir
	e<-saveBackup rpg2
	assertBool ("e is not Right "++(show e)) (case e of 
		(Right _)-> True
		_ -> False)
	games<-listGames rpg name
	assertEqual "not 1 games found" 1 (length games)
	assertBool "head hames is not Right" (case (head games) of 
		(Right _)-> True
		_ -> False)
	let Right (n,_)=head games
	assertEqual "not name.1" (name++".1") n
	currentGameExists<-doesCurrentGameExists rpg name
	assertBool "game does exist" (not currentGameExists)
	chars<-listCharacters rpg
	assertEqual "not 1 character" 1 (length chars)
	assertEqual "not JP" name (head chars) 
	
	lb<-loadBackup rpg n
	assertBool "lb is not Right" (case lb of 
		(Right _)-> True
		_ -> False)
	
	eD<-deleteGame rpg2 n
	assertBool ("eD is not Right: "++(show eD)) (case eD of 
		(Right _)-> True
		_ -> False)
	currentGameExists<-doesCurrentGameExists rpg name
	assertBool "game does exist" (not currentGameExists)
	chars<-listCharacters rpg
	assertEqual "not 1 character" 1 (length chars)
	assertEqual "not JP" name (head chars) 
	games<-listGames rpg name
	assertEqual ("game found "++show (head games)) 0 (length games)
	
	lb2<-loadBackup rpg n
	assertBool "lb2 is not Left" (case lb2 of 
		(Right _)-> False
		(Left _) -> True)
		
	return ()
	))
	
testBackupAndCurrent= TestLabel "testBackupAndCurrent" (TestCase (do
	rpg<-getTestRpg
	let name="JP"
	let c=createTestChar name
	saveCharacter rpg c
	sg<-getStdGen
	gw<-evalRandT (generateGameWorld (10,5)) (ProductionRandom sg)
	let mgs=RPGGameState (gw) (MazeObjects M.empty M.empty) 0
	let rpg2=rpg{rpgCharacter=Just c,mgs=Just mgs}
	e<-saveBackup rpg2
	assertBool ("e is not Right "++(show e)) (case e of 
		(Right _)-> True
		_ -> False)
	let Right(_,nB)=e
	
	gw2<-evalRandT (generateGameWorld (10,5)) (ProductionRandom sg)
	let mgs2=RPGGameState (gw2) (MazeObjects M.empty M.empty) 0
	let rpg3=rpg{rpgCharacter=Just c,mgs=Just mgs2}
	eC<-saveCurrent rpg3
	assertBool ("eC is not Right "++(show eC)) (case eC of 
		(Right _)-> True
		_ -> False)
	let Right(_,nC)=eC
	
	games<-listGames rpg name
	assertEqual "not 2 games found" 2 (length games)
	assertBool "head hames is not Right" (case (head games) of 
		(Right _)-> True
		_ -> False)
	assertBool "second hames is not Right" (case (head $ tail games) of 
		(Right _)-> True
		_ -> False)
		
	lbB<-loadBackup rpg nB
	assertBool "lbB is not Right" (case lbB of 
		(Right _)-> True
		_ -> False)
	
	let Right (_,rpg2a)=lbB
	assertEqual "backup is not right rpg" rpg2 rpg2a
	
	lbC<-loadBackup rpg nC
	assertBool "lbC is not Right" (case lbC of 
		(Right _)-> True
		_ -> False)
	
	let Right (_,rpg3a)=lbC
	assertEqual "Current is not right rpg" rpg3 rpg3a	
	
	eD<-deleteGame rpg2 nB
	assertBool ("eD is not Right: "++(show eD)) (case eD of 
		(Right _)-> True
		_ -> False)
	currentGameExists<-doesCurrentGameExists rpg nB
	assertBool "game does exist" (not currentGameExists)
	chars<-listCharacters rpg
	assertEqual "not 1 character" 1 (length chars)
	assertEqual "not JP" name (head chars) 
	games<-listGames rpg nB
	assertEqual ("game found "++show (head games)) 0 (length games)
	
	lb2<-loadBackup rpg nB
	assertBool "lb2 is not Left" (case lb2 of 
		(Right _)-> False
		(Left _) -> True)
		
	eDC<-deleteGame rpg2 nC
	assertBool ("eDC is not Right: "++(show eDC)) (case eDC of 
		(Right _)-> True
		_ -> False)	
		
	return ()
	
	))


getTestRpg= do
	tmp<-getTemporaryDirectory
	let dir=combine tmp "rpg"
	b<-doesDirectoryExist dir
	when b (removeDirectoryRecursive dir)
	return (RPGState Nothing Nothing dir)

testSaveCurrentOOS= TestLabel "testSaveCurrentOOS" (TestCase (do
	rpg<-getTestRpg
	let name="JP"
	let c=createTestChar name
	saveCharacter rpg c
	sg<-getStdGen
	gw<-evalRandT (generateGameWorld (10,5)) (ProductionRandom sg)
	let mgs=RPGGameState (gw) (MazeObjects M.empty M.empty) 0
	let rpg2=rpg{rpgCharacter=Just c,mgs=Just mgs}
	
	eC<-saveCurrent rpg2
	assertBool ("eC is not Right "++(show eC)) (case eC of 
		(Right _)-> True
		_ -> False)
	let Right(_,nC)=eC
	
	let c2=setCharacteristic' c Current Physical 0

	let rpg3=rpg{rpgCharacter=Just c2}
	
	eC2<-saveCurrent rpg3
	assertBool ("eC2 is not Right "++(show eC2)) (case eC2 of 
		(Right _)-> True
		_ -> False)
	
	currentGameExists<-doesCurrentGameExists rpg nC
	assertBool "game does exist" (not currentGameExists)
	chars<-listCharacters rpg
	assertEqual "not 0 character" 0 (length chars)
	games<-listGames rpg nC
	assertEqual ("game found "++show (head games)) 0 (length games)
	
	))

