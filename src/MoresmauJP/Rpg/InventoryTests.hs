-- | Inventory management hunit tests
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.InventoryTests where

import Data.Maybe

import MoresmauJP.Rpg.CharacterTests
import MoresmauJP.Rpg.Inventory
import MoresmauJP.Rpg.Items

import Test.HUnit

inventoryTests = TestList [
		testInventory,testInventory2Hands,testInventoryPick2Hands,testMakeFullInventory]
		

testInventory = TestLabel "Test Inventory" (TestCase (do
	let i1=mkEmptyInventory
	let Right(i2,item1)=dropItem i1 RightHand
	assertBool "item from empty inventory is not Nothing" (isNothing item1)
	let (Left (InvalidBagPosition 10))=dropItem i2 (Bag 10)
	let sw=sword
	let Right(i3,item2)=takeItem i2 sw RightHand
	assertBool "item from empty inventory is not Nothing" (null item2)
	let items=listItems i3
	assertEqual "Full list is not 14" 14 (length items)
	assertEqual "Head of items is not right hand and sword" (RightHand,Just sw) (head items)
	assertEqual "2nd Head of items is not left hand and empty" (LeftHand,Nothing) (head $ tail items)
	let Right(_,item3)=dropItem i3 RightHand
	assertBool "item from inventory is not Just" (isJust item3)
	assertEqual "item dropped is not sword" (Just sw) (item3) 
	))
	
testInventory2Hands=TestLabel "Test Inventory 2 hands" (TestCase (do
		let i1=mkEmptyInventory
		let sw=twoHandedWord
		let Right(i2,item1)=takeItem i1 sw RightHand
		assertBool "item from empty inventory is not Nothing" (null item1)
		let carriedItems=listCarriedItems i2
		assertEqual "not carrying 2 items" 2 (length carriedItems)
		assertEqual "not carrying 1 unique item" 1 (length $ listCarriedItemsUniqueObject i2)
		assertEqual "not carrying in right hand" 1 (length (filter (\(pos,i)->pos==RightHand && sw==i) carriedItems))
		assertEqual "not carrying in left hand" 1 (length (filter (\(pos,i)->pos==LeftHand && sw==i) carriedItems))
		let Right(i3,item2)=dropItem i2 RightHand
		assertBool "carrying something" (null (listCarriedItems i3))
		assertBool "didn't drop something" (isJust item2)
		assertEqual "didn't drop sword" sw (fromJust item2)
		let Right(i4,item3)=takeItem i3 sw (Bag 1)
		assertEqual "not carrying 1 item" 1 (length (listCarriedItems i4))
		assertBool "dropped something" (null item3)
		let Right(i5,item4)=dropItem i4 (Bag 1)
		assertBool "carrying something" (null (listCarriedItems i5))
		assertBool "didn't drop something" (isJust item4)
		assertEqual "didn't drop sword" sw (fromJust item4)
		let Right(i6,_)=takeItem i5 sw RightHand
		let sw2=battleaxe
		let Right(i7,item6)=takeItem i6 sw2 RightHand
		assertEqual "dropped one item" 0 (length item6)
		assertEqual "not carrying 2 things (including an axe occupying 2 hands)" 3 (length (listCarriedItems i7))
		let (Right mi)=getCarriedItem i7 (Bag 1)
		assertBool "nothing in bag 1" (isJust mi)
		assertEqual "no sword in bag 1" sw (fromJust mi)
		let (Right mi2)=getCarriedItem i7 (Bag 2)
		assertBool "something in bag 2" (not $ isJust mi2)
		
	))	
	
testInventoryPick2Hands=TestLabel "Test Inventory Pick 2 handed weapon" (TestCase (do	
		let i1=mkEmptyInventory
		let sw=twoHandedWord
		let sw1=sword
		let sw2=dagger
		let Right(i2,item1)=takeItem i1 sw1 RightHand
		assertBool "item from empty inventory is not Nothing" (null item1)
		let Right(i3,item2)=takeItem i2 sw2 LeftHand
		assertBool "item from empty inventory is not Nothing" (null item2)
		assertEqual "not carrying 2 items" 2 (length $ listCarriedItems i3)
		assertEqual "not carrying 2 unique items" 2 (length $ listCarriedItemsUniqueObject i3)
		let Right(i4,item3)=takeItem i3 sw RightHand
		assertBool "item from empty inventory is not Nothing" (null item3)
		assertEqual "not carrying 4 items" 4 (length $ listCarriedItems i4)
		assertEqual "not carrying 3 unique items" 3 (length $ listCarriedItemsUniqueObject i4)
		assertEqual "not carrying 2 handed sword in right hand" (Right $ Just sw) (getCarriedItem i4 RightHand)
		assertEqual "not carrying 2 handed sword in left hand" (Right $ Just sw) (getCarriedItem i4 LeftHand)
		assertEqual "not carrying sword in bag" (Right $ Just sw1) (getCarriedItem i4 (Bag 1))
		assertEqual "not carrying dagger in bag" (Right $ Just sw2) (getCarriedItem i4 (Bag 2))
	))
	
testMakeFullInventory= TestLabel "Test makeFullInventory" (TestCase (do
	let pos1=[(RightHand,sword),(Body,leatherArmor),(Bag 1,minorHealingPotion)]
	let i1=makeFullInventory pos1 1 0
	assertEqual "not carrying 3" 3 (length $ listCarriedItems i1)
	assertEqual "not carrying sword in right hand" (Right $ Just sword) (getCarriedItem i1 RightHand)
	assertEqual "not carrying leatherarmor on body" (Right $ Just leatherArmor) (getCarriedItem i1 Body)
	assertEqual "not carrying minor healing potion on bag 1" (Right $ Just minorHealingPotion) (getCarriedItem i1 (Bag 1))
	let pos2=[(RightHand,twoHandedWord),(LeftHand,sword),(Body,leatherArmor)]
	let i2=makeFullInventory pos2 0 0
	assertEqual "not carrying 3" 3 (length $ listCarriedItems i2)
	assertEqual "not carrying 2hand sword in right hand" (Right $ Just twoHandedWord) (getCarriedItem i2 RightHand)
	assertEqual "not carrying 2hand sword in left hand" (Right $ Just twoHandedWord) (getCarriedItem i2 LeftHand)
	assertEqual "not carrying leatherarmor on body" (Right $ Just leatherArmor) (getCarriedItem i2 Body)
	))
			