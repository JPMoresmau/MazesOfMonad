-- | Fight resolution hunit tests
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.FightTests where

import Control.Monad.Writer

import MoresmauJP.Rpg.Character
import MoresmauJP.Rpg.CharacterTests
import MoresmauJP.Rpg.Fight
import MoresmauJP.Rpg.Inventory

import MoresmauJP.Util.Random

import Test.HUnit

fightTests = TestList [testFightRoundNormal,
		testFightRoundFumbleSelfWound,testFightRoundFumbleDexterityLoss,
		testFightRoundFumbleCharismaLoss,testFightRoundFumbleWeaponBreak,testFightRoundFumbleWeaponBreakNoWeapon,
		testFightRoundKill]

testFightRoundNormal=TestLabel "Test Fight Round Normal Hit" (TestCase (do
	--let jpChars=unlines (map (\x-> (show x) ++ " 10/10(0)") [Strength .. Mental])
	
	let jp=createTestChar "JP"
	assertBool "JP is not OK" (isOK jp)
	let troll=createTestChar "Troll"
	assertBool "Troll is not OK" (isOK troll)
	
	(((jp2,troll2),dead),msgs)<-evalRandT (runWriterT $ giveBlow jp troll) (mkTestWrapper[8])--processFightRound jp troll [11,8,11]
	assertBool "somebody is dead!" (not dead)
	assertEqual "jp2 is not jp" (name jp2) (name jp)
	assertEqual "troll2 is not troll" (name troll2) (name troll)
	assertBool "JP is not OK" (isOK jp2)
	assertBool "Troll is not OK" (isOK troll2)
	assertEqual "jp2 physical is not jp physical" (getCharacteristic' jp2 Current Physical) (getCharacteristic' jp Current Physical)
	assertEqual "troll didn't lose 2 physical" (getCharacteristic' troll2 Current Physical) ((getCharacteristic' troll Current Physical)-2)
	assertEqual "Not 1 message" 1 (length msgs)
	assertEqual "Msg1" "JP hits and causes 2 damages" (head msgs)
	--assertEqual "Msg2" "Troll misses" (head $ tail msgs)
	))
	



testFightRoundFumbleSelfWound=TestLabel "Test Fight Round Fumbles SelfWound" (TestCase (do
	--let jpChars=unlines (map (\x-> (show x) ++ " 10/10(0)") [Strength .. Mental])
	let jp=createTestChar "JP"
	assertBool "JP is not OK" (isOK jp)
	let troll=createTestChar "Troll"
	assertBool "Troll is not OK" (isOK troll)
	(((troll2,jp2),dead),msgs)<-evalRandT (runWriterT $ giveBlow troll jp) (mkTestWrapper [20]) --processFightRound jp troll [11,11,20]
	assertBool "somebody is dead!" (not dead)
	assertEqual "jp2 is not jp" (name jp2) (name jp)
	assertEqual "troll2 is not troll" (name troll2) (name troll)
	assertBool "JP is not OK" (isOK jp2)
	assertBool "Troll is not OK" (isOK troll2)
	assertEqual "jp2 physical is not jp physical" (getCharacteristic' jp2 Current Physical) (getCharacteristic' jp Current Physical)
	assertEqual "troll didn't lose 2 physical" ((getCharacteristic' troll Current Physical)-2) (getCharacteristic' troll2 Current Physical)
	assertEqual "Not 1 message" 1 (length msgs)
	--assertEqual "Msg1" "JP misses" (head msgs)
	assertEqual "Msg2" "Troll fumbles and gives himself 2 damages" (head msgs)
	))
	
testFightRoundFumbleDexterityLoss=TestLabel "Test Fight Round Fumbles DexterityLoss" (TestCase (do
	let jp=createTestChar "JP"
	assertBool "JP is not OK" (isOK jp)
	let troll=(createTestChar "Troll"){gender=Female}
	assertBool "Troll is not OK" (isOK troll)
	(((troll2,jp2),dead),msgs)<-evalRandT (runWriterT $ giveBlow troll jp) (mkTestWrapper [20,1,20,20,20,20,20]) 
	assertBool "somebody is dead!" (not dead)
	assertEqual "jp2 is not jp" (name jp2) (name jp)
	assertEqual "troll2 is not troll" (name troll2) (name troll)
	assertBool "JP is not OK" (isOK jp2)
	assertBool "Troll is not OK" (isOK troll2)
	assertEqual "jp2 physical is not jp physical" (getCharacteristic' jp2 Current Physical) (getCharacteristic' jp Current Physical)
	assertEqual "troll didn't lose 1 physical" ((getCharacteristic' troll Current Physical)-1) (getCharacteristic' troll2 Current Physical)
	assertEqual "troll didn't lose 1 dexterity" ((getCharacteristic' troll Current Dexterity)-1) (getCharacteristic' troll2 Current Dexterity)
	
	assertEqual "Not 1 message" 1 (length msgs)
	assertEqual "Msg2" "Troll fumbles and gives herself a hand injury (-1 Dexterity)" (head msgs)
	))	

testFightRoundFumbleCharismaLoss=TestLabel "Test Fight Round Fumbles CharismaLoss" (TestCase (do
	let jp=createTestChar "JP"
	assertBool "JP is not OK" (isOK jp)
	let troll=(createTestChar "Troll"){gender=Female}
	assertBool "Troll is not OK" (isOK troll)
	(((troll2,jp2),dead),msgs)<-evalRandT (runWriterT $ giveBlow troll jp) (mkTestWrapper [20,20,1,20,20,20,20]) 
	assertBool "somebody is dead!" (not dead)
	assertEqual "jp2 is not jp" (name jp2) (name jp)
	assertEqual "troll2 is not troll" (name troll2) (name troll)
	assertBool "JP is not OK" (isOK jp2)
	assertBool "Troll is not OK" (isOK troll2)
	assertEqual "jp2 physical is not jp physical" (getCharacteristic' jp2 Current Physical) (getCharacteristic' jp Current Physical)
	assertEqual "troll didn't lose 1 physical" ((getCharacteristic' troll Current Physical)-1) (getCharacteristic' troll2 Current Physical)
	assertEqual "troll didn't lose 1 charisma" ((getCharacteristic' troll Current Charisma)-1) (getCharacteristic' troll2 Current Charisma)
	
	assertEqual "Not 1 message" 1 (length msgs)
	assertEqual "Msg2" "Troll fumbles and gives herself a face injury (-1 Charisma)" (head msgs)
	))	

testFightRoundFumbleWeaponBreak=TestLabel "Test Fight Round Fumbles WeaponBreak" (TestCase (do
	let jp=createTestChar "JP"
	assertBool "JP is not OK" (isOK jp)
	let Right (i,_)=takeItem mkEmptyInventory (Weapon "Massive Club" 3 10 2 4) RightHand
	let troll=(createTestChar "Troll"){gender=Female,inventory=i}
	assertBool "Troll is not OK" (isOK troll)
	(((troll2,jp2),dead),msgs)<-evalRandT (runWriterT $ giveBlow troll jp) (mkTestWrapper [20,20,20,1,20,20,20]) 
	assertBool "somebody is dead!" (not dead)
	assertEqual "jp2 is not jp" (name jp2) (name jp)
	assertEqual "troll2 is not troll" (name troll2) (name troll)
	assertBool "JP is not OK" (isOK jp2)
	assertBool "Troll is not OK" (isOK troll2)
	assertEqual "jp2 physical is not jp physical" (getCharacteristic' jp2 Current Physical) (getCharacteristic' jp Current Physical)
	assertEqual "troll2 physical is not troll physical" (getCharacteristic' troll Current Physical) (getCharacteristic' troll2 Current Physical)
	assertEqual "troll2 charisma is not troll charisma" (getCharacteristic' troll Current Charisma) (getCharacteristic' troll2 Current Charisma)
	assertEqual "troll2 dexterity is not troll dexterity" (getCharacteristic' troll Current Dexterity) (getCharacteristic' troll2 Current Dexterity)
	
	assertEqual "Not 1 message" 1 (length msgs)
	assertEqual "Msg2" "Troll fumbles and breaks her Massive Club" (head msgs)
	))	

testFightRoundFumbleWeaponBreakNoWeapon=TestLabel "Test Fight Round Fumbles WeaponBreakNoWeapon" (TestCase (do
	let jp=createTestChar "JP"
	assertBool "JP is not OK" (isOK jp)
	let troll=(createTestChar "Troll"){gender=Female}
	assertBool "Troll is not OK" (isOK troll)
	(((troll2,jp2),dead),msgs)<-evalRandT (runWriterT $ giveBlow troll jp) (mkTestWrapper [20,20,20,1,20,20,20]) 
	assertBool "somebody is dead!" (not dead)
	assertEqual "jp2 is not jp" (name jp2) (name jp)
	assertEqual "troll2 is not troll" (name troll2) (name troll)
	assertBool "JP is not OK" (isOK jp2)
	assertBool "Troll is not OK" (isOK troll2)
	assertEqual "jp2 physical is not jp physical" (getCharacteristic' jp2 Current Physical) (getCharacteristic' jp Current Physical)
	assertEqual "troll didn't lose 1 physical" ((getCharacteristic' troll Current Physical)-1) (getCharacteristic' troll2 Current Physical)
	assertEqual "troll didn't lose 1 dexterity" ((getCharacteristic' troll Current Dexterity)-1) (getCharacteristic' troll2 Current Dexterity)
	
	assertEqual "Not 1 message" 1 (length msgs)
	assertEqual "Msg2" "Troll fumbles and gives herself a hand injury (-1 Dexterity)" (head msgs)
	))	

testFightRoundKill=TestLabel "Test Fight Round Kill" (TestCase (do
	--let jpChars=unlines (map (\x-> (show x) ++ " 10/10(0)") [Strength .. Mental])
	let jp=createTestChar "JP"
	assertBool "JP is not OK" (isOK jp)
	let troll1=createTestChar "Troll"
	let troll=setCharacteristic' troll1 Current Physical 2
	assertBool "Troll is not OK" (isOK troll)
	(((jp2,troll2),dead),msgs)<-evalRandT (runWriterT $ giveBlow jp troll) (mkTestWrapper [8]) -- processFightRound jp troll [11,8,11]
	assertBool "nobody is dead!" dead
	assertEqual "jp2 is not jp" (name jp2) (name jp)
	assertEqual "troll2 is not troll" (name troll2) (name troll)
	assertBool "JP is not OK" (isOK jp2)
	assertBool "Troll is  OK" (not (isOK troll2))
	assertBool "Troll is not dead" (isDead troll2)
	assertBool "Troll is not oos" (isOutOfService troll2)
	assertEqual "jp2 physical is not jp physical" (getCharacteristic' jp2 Current Physical) (getCharacteristic' jp Current Physical)
	assertEqual "troll didn't lose 2 physical" ((getCharacteristic' troll Current Physical)-2) (getCharacteristic' troll2 Current Physical)
	assertEqual "Not 1 message" (length msgs) 1
	assertEqual "Msg1" "JP hits and causes 2 damages" (head msgs)
	))