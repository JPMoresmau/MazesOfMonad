-- | Items hunit tests
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.ItemsTests where

import Control.Monad.Writer
import Data.Maybe

import MoresmauJP.Rpg.Character
import MoresmauJP.Rpg.CharacterTests
import MoresmauJP.Rpg.Inventory
import MoresmauJP.Rpg.Items

import MoresmauJP.Util.Random

import Test.HUnit

itemTests=TestList [testUseHealingPotion,testUseMindPotion,testUseWeapon,testUseScrollSuccess,testUseScrollFailure,testUseScrollFumble]	

testUseHealingPotion = TestLabel "Test Use Healing Potion" (TestCase (do
	let jp=createTestChar "JP"
	assertEqual "Physical must be 10" 10 (getCharacteristic' jp Current Physical)
	let jp2=setCharacteristic' jp Current Physical 5
	((Just (jp3,remove1)),msgs)<-evalRandT (runWriterT (useItemEffect minorHealingPotion jp2)) (mkTestWrapper [8])
	assertEqual "not recover message1" "You recover 3 Physical points!" (head msgs)
	assertEqual "Physical must be 8 after potion" 8 (getCharacteristic' jp3 Current Physical)
	assertBool "not remove1" remove1
	((Just (jp4,remove2)),msgs2)<-evalRandT (runWriterT (useItemEffect minorHealingPotion jp3)) (mkTestWrapper [8])
	assertEqual "not recover message2" "You recover 2 Physical points!" (head msgs2)
	assertEqual "Physical must be 10 after potion" 10 (getCharacteristic' jp4 Current Physical)
	assertBool "not remove2" remove2
	)) 
	
testUseMindPotion = TestLabel "Test Use Mind Potion" (TestCase (do
	let jp=createTestChar "JP"
	assertEqual "Mental must be 10" 10 (getCharacteristic' jp Current Mental)
	let jp2=setCharacteristic' jp Current Mental 5
	((Just (jp3,remove1)),msgs)<-evalRandT (runWriterT (useItemEffect minorMindPotion jp2)) (mkTestWrapper [8])
	assertEqual "not recover message1" "You recover 3 Mental points!" (head msgs)
	assertEqual "Mental must be 8 after potion" 8 (getCharacteristic' jp3 Current Mental)
	assertBool "not remove1" remove1
	((Just (jp4,remove2)),msgs2)<-evalRandT (runWriterT (useItemEffect minorMindPotion jp3)) (mkTestWrapper [8])
	assertEqual "not recover message2" "You recover 2 Mental points!" (head msgs2)
	assertEqual "Mental must be 10 after potion" 10 (getCharacteristic' jp4 Current Mental)
	assertBool "not remove2" remove2
	)) 	

testUseWeapon = TestLabel "Test Use Weapon" (TestCase (do
	let jp=createTestChar "JP"
	assertEqual "Physical must be 10" 10 (getCharacteristic' jp Current Physical)
	let jp2=setCharacteristic' jp Current Physical 5
	(r,_)<-evalRandT (runWriterT (useItemEffect battleaxe jp2)) (mkTestWrapper [10])
	assertBool "result is not Nothing" (isNothing r)
	)) 
	
testUseScrollSuccess = TestLabel "Test Use Scroll Success" (TestCase (do
	let jp=createTestChar "JP"
	assertEqual "spells must be empty" 0 (length $ spells jp)
	let sc=Scroll "Scroll of nimble fingers" "Nimble Fingers" 5
	((Just (jp2,remove1)),(s:[]))<-evalRandT (runWriterT (useItemEffect sc jp)) (mkTestWrapper [7])
	assertBool "not remove1" remove1
	assertEqual "not success message" "You learn the spell Nimble Fingers" s
	assertEqual "spells2 must be one" 1 (length $ spells jp2)
	assertEqual "spell is not Nimble Fingers" "Nimble Fingers" (spellName $ head $ spells jp2)
	((Just (jp3,remove2)),(s:[]))<-evalRandT (runWriterT (useItemEffect sc jp2)) (mkTestWrapper [9])
	assertBool "remove2" (not remove2)
	assertEqual "not already message" "You already know that spell" s
	assertEqual "spells2 must be one" 1 (length $ spells jp3)
	assertEqual "spell is not Nimble Fingers" "Nimble Fingers" (spellName $ head $ spells jp3)
	))
	
testUseScrollFailure = TestLabel "Test Use Scroll Failure" (TestCase (do
	let jp=createTestChar "JP"
	assertEqual "spells must be empty" 0 (length $ spells jp)
	let sc=Scroll "Scroll of nimble fingers" "Nimble Fingers" 5
	((Just (jp2,remove1)),(s:[]))<-evalRandT (runWriterT (useItemEffect sc jp)) (mkTestWrapper [8])
	assertBool "not remove1" remove1
	assertEqual "not success message" "You fail to learn that spell" s
	assertEqual "spells2 must be empty" 0 (length $ spells jp2)
	))
	
testUseScrollFumble = TestLabel "Test Use Scroll Failure" (TestCase (do
	let jp=createTestChar "JP"
	assertEqual "spells must be empty" 0 (length $ spells jp)
	let sc=Scroll "Scroll of nimble fingers" "Nimble Fingers" 5
	((Just (jp2,remove1)),(s:[]))<-evalRandT (runWriterT (useItemEffect sc jp)) (mkTestWrapper [20])
	assertBool "not remove1" remove1
	assertEqual "not success message" "You fail badly to learn that spell" s
	assertEqual "spells2 must be empty" 0 (length $ spells jp2)
	assertEqual "Mental must be 9" 9 (getCharacteristic' jp2 Current Mental)
	))	