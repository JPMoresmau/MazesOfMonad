-- | Character handling unit tests
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.CharacterTests where

import Data.Array.IArray

import MoresmauJP.Rpg.Character
import MoresmauJP.Rpg.Inventory
import MoresmauJP.Rpg.Profile

import MoresmauJP.Util.Random

import Test.HUnit

import System.Random

characterTests=TestList [testLevel,testSetOOS,testExperience,testAffects,testRestoreWithTime]

createTestChar :: String -> Character
createTestChar name=Character name Male 
	(array (Strength,Mental) 
		(map (\x->(x,(Rating 
			(array (Normal,Experience) [(Normal,10),(Current,10),(Experience,0)])
			))) 
			[Strength .. Mental]
		)
	) mkEmptyInventory [] []
	
mkEmptyInventory=(makeEmptyInventory 10 0)

	
testLevel= TestLabel "Test Level" (TestCase (do
	let jp=createTestChar "JP"
	assertEqual "Level is not 8" 8 (characterLevel jp)
	let jp2=Character "jp2" Male (array (Strength,Mental) 
			(map (\x->(x,(Rating 
				(array (Normal,Experience) [(Normal,15),(Current,10),(Experience,5)])
				))) 
				[Strength .. Mental]
			)
		) mkEmptyInventory [] []
	assertEqual "Level is not 13" 13 (characterLevel jp2)	
	let jp3=jp2{spells=[Spell "Feel Better" Physical Recovery Permanent
		,Spell "Fire Ball" Physical Negative Permanent
		,Spell "Nimble Fingers" Dexterity Positive Temporary
		,Spell "Greasy Fingers" Dexterity Negative Temporary]}
	assertEqual "Level is not 14" 14 (characterLevel jp3)
	let (Right (i2,_))=takeItem (inventory jp3) (Weapon "Sword" 2 6 1 10) (RightHand)
	let (Right (i3,_))=takeItem i2 (Weapon "Sword" 2 6 1 10) (LeftHand)
	let (Right (i4,_))=takeItem i3 (Weapon "Sword" 2 6 1 10) (Bag 1)
	let (Right (i5,_))=takeItem i4 (Weapon "Sword" 2 6 1 10) (Bag 2)
	let jp4=jp3{inventory=i5}
        assertEqual "Level is not 17" 17 (characterLevel jp4)
	let jp5=setCharacteristic' jp4 Normal Willpower 5
	assertEqual "Level is not 17" 17 (characterLevel jp5)
	
	))
	

testSetOOS = TestLabel "Test Out Of Service" (
	TestCase (do
		sg<-getStdGen
		mt<-evalRandT(generateTraits $ head $ profiles) (ProductionRandom sg)
		let c=Character "Test" Male (getDefaultHealth mt) mkEmptyInventory [] []
		assertBool "Character is not OK" (isOK c)
		assertBool "Character is out of service" (not (isOutOfService c))
		let c2=setCharacteristic' c Current Physical 0
		assertBool "Character is not dead" (isDead c2)
		assertBool "Character is mad" (not (isMad c2))
		assertBool "Character is ok" (not (isOK c2))
		assertBool "Character is not out of service" (isOutOfService c2)
		let c3=setCharacteristic' c2 Current Physical 10
		let c4=setCharacteristic' c3 Current Mental 0
		assertBool "Character is not mad" (isMad c4)
		assertBool "Character is dead" (not (isDead c4))
		assertBool "Character is ok" (not (isOK c4))
		assertBool "Character is not out of service" (isOutOfService c4)
		let c5=setCharacteristic' c4 Current Physical 0
		assertBool "Character is not mad" (isMad c5)
		assertBool "Character is not dead" (isDead c5)
		assertBool "Character is ok" (not (isOK c5))
		assertBool "Character is not out of service" (isOutOfService c5)
	))
	

testExperience = TestLabel "Test Experience" (TestCase (do
	let jp=createTestChar "JP"
	assertEqual "current dexterity is not 10" 10 (getCharacteristic' jp Current Dexterity)
	assertEqual "normal dexterity is not 11" 10 (getCharacteristic' jp Normal Dexterity)
	assertEqual "experience dexterity is not 0" 0 (getCharacteristic' jp Experience Dexterity)
	let jp2=setCharacteristic' jp Experience Dexterity 334
	assertEqual "current dexterity is not 11" 11 (getCharacteristic' jp2 Current Dexterity)
	assertEqual "normal dexterity is not 11" 11 (getCharacteristic' jp2 Normal Dexterity)
	assertEqual "experience dexterity is not 1" 1 (getCharacteristic' jp2 Experience Dexterity)
	))		
	
testAffects= TestLabel "Test Affects" (TestCase (do
	let 
		jp=createTestChar "JP"
		(jp',descs)=expireAffects jp 1
	assertEqual "jp has changed" jp jp'
	assertEqual "descs found while no affect" 0 (length descs)
	
	let 
		aff1=Affect Strength (-3) 5 "weakness" "under a spell of weakness" "you feel stronger"
		aff2=Affect Dexterity 2 10 "nimble fingers" "under a spell of nimble fingers" "you feel as clumsy as usual"
		jp2=addAffect (addAffect jp aff1) aff2
		(jp3,descs1)=expireAffects jp2 1
		(jp4,descs2)=expireAffects jp3 5
		(jp5,descs3)=expireAffects jp4 11
	
	assertEqual "jp2 has changed" jp2 jp3
	assertEqual "descs found while tick is <" 0 (length descs1)
	assertEqual "strength is not 7" 7 (getCharacteristic' jp3 Current Strength)
	assertEqual "dexterity is not 12" 12 (getCharacteristic' jp3 Current Dexterity)
	
	assertEqual "descs2 not found while tick is >" 1 (length descs2)
	assertEqual "not feel stronger" "you feel stronger" (head descs2)
	assertEqual "jp4 has not 1 affect left" 1 (length $ affects jp4)
	assertEqual "jp4 does not have dexterity affect left" aff2 (head $ affects jp4)
	assertEqual "jp5 has not 10 strength" 10 (getCharacteristic' jp5 Current Strength)
	
	assertEqual "descs3 not found while tick is >" 1 (length descs3)
	assertEqual "not feel clumsy" "you feel as clumsy as usual" (head descs3)
	assertEqual "jp5 has not 0 affect left" 0 (length $ affects jp5)
	assertEqual "jp5 has not 10 dexterity" 10 (getCharacteristic' jp5 Current Dexterity)
	
	))
	
testRestoreWithTime= TestLabel "Test RestoreWithTime" (TestCase (do
	let 
		jp=setCharacteristic' (createTestChar "JP") Current Physical 8
		jp2=restoreWithTime jp 100 50
		jp3=restoreWithTime jp 100 200
		jp4=restoreWithTime jp 100 400
		jp5=restoreWithTime jp 100 600
		jp6=restoreWithTime jp 300 100
	assertEqual "jp2 should still have 8" 8 (getCharacteristic' jp2  Current Physical)
	assertEqual "jp3 should have 9" 9 (getCharacteristic' jp3  Current Physical)
	assertEqual "jp4 should have 9" 9 (getCharacteristic' jp4  Current Physical)
	assertEqual "jp5 should have 10" 10 (getCharacteristic' jp5  Current Physical)
	assertEqual "jp6 should still have 8" 8 (getCharacteristic' jp6  Current Physical)
	
	let 
		jp=setCharacteristic' (setCharacteristic' (createTestChar "JP") Current Physical 8) Current Constitution 15
		jp2=restoreWithTime jp 100 50
		jp3=restoreWithTime jp 100 200
		jp4=restoreWithTime jp 100 400
		jp5=restoreWithTime jp 100 600
		jp6=restoreWithTime jp 300 50
	assertEqual "jp2 should still have 8" 8 (getCharacteristic' jp2  Current Physical)
	assertEqual "jp3 should have 9" 9 (getCharacteristic' jp3  Current Physical)
	assertEqual "jp4 should have 10" 10 (getCharacteristic' jp4  Current Physical)
	assertEqual "jp5 should have 10" 10 (getCharacteristic' jp5  Current Physical)
	assertEqual "jp6 should still have 8" 8 (getCharacteristic' jp6  Current Physical)
	
	
	))
