-- | NPC templates hunit tests
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.NPCTests where

import Data.List
import Control.Monad

import MoresmauJP.Rpg.Character
import MoresmauJP.Rpg.CharacterTests
import MoresmauJP.Rpg.Inventory
import MoresmauJP.Rpg.NPC
import MoresmauJP.Util.Random

import Test.HUnit

import System.Random

npcTests = TestList [testNPCTemplate,testNPCOccurences,testFightAttitudes,
	testFightAttitudesAlready,testFightAttitudesAnimal]

createTestNPC :: String -> NPCCharacter
createTestNPC name=NPCCharacter (createTestChar name) Human 10


testNPCTemplate=TestLabel "Test NPC Template" (TestCase (do
	mapM_ (\template-> do
		replicateM_ 5 (
			do
				sg<-getStdGen
				NPCCharacter{npcCharacter=t}<-evalRandT (generateFromTemplate template 0) (ProductionRandom sg)
				mapM_ (testNPCTraits t) (traitRanges template)
				mapM_ (testNPCInventory t) (possibleItems template)
			)
		) allNPCTemplates))
	
testNPCTraits :: Character -> (Characteristic,(Int,Int)) -> IO()
testNPCTraits c (char,(low,high)) = do
	let v=getCharacteristic' c Current char
	assertBool "Value not within range" (v>=low && v<=high)
	

testNPCInventory :: Character -> (Position,[(Maybe ItemType,Int)]) -> IO()
testNPCInventory c (pos,items)= do
	let item=getCarriedItem (inventory c) pos
	assertBool ("Incorrect pos "++(show pos)) (case item of {Right _ -> True;_->False})
	let item'=case item of {Right i -> i;_->error "no right"}
	assertBool ("incorrect position for item "++(show item')++ " in pos "++ (show pos)) (case item' of
		Nothing-> True
		Just i -> positionAllowed i pos)
	case (pos,item') of
		(LeftHand,Just Weapon {hands=2})-> return ()
		_ -> assertBool ("Item not from template: "++(name c)++":"++(show item')) (elem  item' (map fst items)) 
	

testNPCOccurences= TestLabel "Test NPCOccurences" (TestCase (do
	let allTemplates=allNPCTemplates
	mapM_ (\template -> do 
			let level=templateLevel template
			let levelUp=div (level * 10) 8
			let levelDown=div (levelUp * 8) 10
			if levelDown==level
				then do
					let occs=getNPCOccurences levelUp
					let maxTemplate=maximumBy (\t1 t2->compare (snd t1) (snd t2)) occs
					let allMax=filter (\t1->(snd maxTemplate)==( snd t1))occs
					--mapM_  (\a->print ((show a)++(show $ templateLevel $ fst a))) allMax
					assertBool ("template "++(typeName template)++ " is not maximum (level:"++(show level)) (elem (template) (map fst allMax))
				else
					return () 
		) allTemplates
	))	
	

testFightAttitudes=TestLabel "Test Fight Attitudes" (TestCase (do
	let npc1=createTestNPC "NPC"
	let vc1=addCharacteristic' ((npcCharacter npc1){inventory=makeFullInventory [] 10 100}) Current Physical (-5)
	assertEqual "not 50%" 50 (getCurrentPercentOfNormal vc1 Physical)
	let npc=npc1{npcCharacter=vc1}
	let fightInfo=addCharacterLoss (addCharacterLoss
			(addNPCLosses newFightInfo npc1 npc)
	 	Physical 1) Physical 1
	let attitudes=getFightAttitudes npc fightInfo
	assertEqual "not 4 attitudes" 4 (length attitudes)
	let expected=[(ContinueFight,11),(TryEscape,6),(PrayForClemency,3),(OfferBribe 100,3)]
	assertEqual "not expected" expected attitudes
	))
	
testFightAttitudesAlready=TestLabel "Test Fight Attitudes Excluding Already Chosen" (TestCase (do
	let npc1=createTestNPC "NPC"
	let vc1=addCharacteristic' ((npcCharacter npc1){inventory=makeFullInventory [] 10 100}) Current Physical (-5)
	assertEqual "not 50%" 50 (getCurrentPercentOfNormal vc1 Physical)
	let npc=npc1{npcCharacter=vc1}
	let fightInfo=(addCharacterLoss (addCharacterLoss
			(addNPCLosses newFightInfo npc1 npc)
	 	Physical 1) Physical 1){hasBribed=True,hasPrayed=True}
	let attitudes=getFightAttitudes npc fightInfo
	assertEqual "not 4 attitudes" 4 (length attitudes)
	let expected=[(ContinueFight,11),(TryEscape,6),(PrayForClemency,0),(OfferBribe 100,0)]
	assertEqual "not expected" expected attitudes
	))
	
testFightAttitudesAnimal=TestLabel "Test Fight Attitudes Animal" (TestCase (do
	let npc1=(createTestNPC "NPC"){npcType=Animal}
	let vc1=addCharacteristic' ((npcCharacter npc1){inventory=makeFullInventory [] 10 100}) Current Physical (-5)
	assertEqual "not 50%" 50 (getCurrentPercentOfNormal vc1 Physical)
	let npc=npc1{npcCharacter=vc1}
	let fightInfo=(addCharacterLoss (addCharacterLoss
			(addNPCLosses newFightInfo npc1 npc)
	 	Physical 1) Physical 1)
	let attitudes=getFightAttitudes npc fightInfo
	assertEqual "not 4 attitudes" 4 (length attitudes)
	let expected=[(ContinueFight,11),(TryEscape,6),(PrayForClemency,0),(OfferBribe 100,0)]
	assertEqual "not expected" expected attitudes
	))