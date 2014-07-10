-- | Action resolution hunit tests
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.ActionsTests where

import Control.Monad.Writer

import MoresmauJP.Rpg.Actions
import MoresmauJP.Rpg.Character
import MoresmauJP.Rpg.CharacterTests

import MoresmauJP.Util.Random

import Test.HUnit
import Text.Printf

actionsTests=TestList [testDifficultyLevel,testEvalResults,testActionCausesLevel]

testDifficultyLevel = TestLabel "testDifficultyLevel" (TestCase (do
	--setTestGen [10]
	let jp=createTestChar "JP"
	let sg=mkTestWrapper [10]
	((_,rr),_)<-evalRandT (runWriterT $ action jp [Dexterity] (toIntLevel Neutral)) sg
	assertEqual "Not standard success 0" (Success Standard 0) rr
	((_,rr),_)<-evalRandT (runWriterT $ action jp [Dexterity] (toIntLevel RatherEasy)) sg
	assertEqual "Not standard success 3" (Success Standard 3) rr
	((_,rr),_)<-evalRandT (runWriterT $ action jp [Dexterity] (toIntLevel Easy)) sg
	assertEqual "Not standard success 6" (Success Standard 6) rr
	((_,rr),_)<-evalRandT (runWriterT $ action jp [Dexterity] (toIntLevel VeryEasy)) sg
	assertEqual "Not standard success 9" (Success Standard 9) rr
	((_,rr),_)<-evalRandT (runWriterT $ action jp [Dexterity] (toIntLevel NearUnmissable)) sg
	assertEqual "Not standard success 10" (Success Standard 10) rr
		
	((_,rr),_)<-evalRandT (runWriterT $ action jp [Dexterity] (toIntLevel RatherHard)) sg
	assertEqual "Not standard failure 3" (Failure Standard 3) rr
	((_,rr),_)<-evalRandT (runWriterT $ action jp [Dexterity] (toIntLevel Hard)) sg
	assertEqual "Not standard failure 6" (Failure Standard 6) rr
	((_,rr),_)<-evalRandT (runWriterT $ action jp [Dexterity] (toIntLevel VeryHard)) sg
	assertEqual "Not standard failure 9" (Failure Standard 9) rr
	((_,rr),_)<-evalRandT (runWriterT $ action jp [Dexterity] (toIntLevel NearImpossible)) sg
	assertEqual "Not standard failure 9" (Failure Standard 9) rr
			
	))
	

testEvalResults = TestList [
		testEvalResult 1 10 (Success Exceptional 9)
		,testEvalResult 3 10 (Success Remarkable 7)
		,testEvalResult 8 10 (Success Standard 2)
		,testEvalResult 11 10 (Failure Standard 1)
		,testEvalResult 17 10 (Failure Remarkable 7)
		,testEvalResult 19 10 (Failure Exceptional 9)
	]
	
testEvalResult roll score expected= TestLabel (printf "testEvalResult %d/%d" roll score)
	(TestCase (do
		let actual = fst (evalResult roll score)
		let msg= printf "%d/%d should be %s but is %s " roll score (show expected) (show actual)
		assertEqual msg expected actual
		))
		
testActionCausesLevel=  TestLabel "testActionCausesLevel" (TestCase (do
	let jp=createTestChar "JP"
	assertEqual "current dexterity is not 10" 10 (getCharacteristic' jp Current Dexterity)
	assertEqual "normal dexterity is not 10" 10 (getCharacteristic' jp Normal Dexterity)
	assertEqual "experience dexterity is not 0" 0 (getCharacteristic' jp Experience Dexterity)
	let jp2=setCharacteristic' jp Experience Dexterity 333
	assertEqual "current dexterity is not 10" 10 (getCharacteristic' jp2 Current Dexterity)
	assertEqual "normal dexterity is not 10" 10 (getCharacteristic' jp2 Normal Dexterity)
	assertEqual "experience dexterity is not 333" 333 (getCharacteristic' jp2 Experience Dexterity)
	let sg=mkTestWrapper [10]
	((jp3,_),msgs)<-evalRandT (runWriterT $ action jp2 [Dexterity] (toIntLevel Neutral)) sg
	assertEqual "current dexterity is not 11" 11 (getCharacteristic' jp3 Current Dexterity)
	assertEqual "normal dexterity is not 11" 11 (getCharacteristic' jp3 Normal Dexterity)
	assertEqual "experience dexterity is not 20" 20 (getCharacteristic' jp3 Experience Dexterity)
	assertEqual "not 1 message" 1 (length msgs)
	assertEqual "not right message" "JP gains 1 point in Dexterity" (head msgs)
	))
	