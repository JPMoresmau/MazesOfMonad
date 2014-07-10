-- | trade hunit tests
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.TradeTests where

import MoresmauJP.Rpg.Actions
import MoresmauJP.Rpg.Character
import MoresmauJP.Rpg.CharacterTests
import MoresmauJP.Rpg.Items
import MoresmauJP.Rpg.Inventory
import MoresmauJP.Rpg.Trade

import Test.HUnit

	
tradeTests=TestList [testEmpty,testBuy,testBuyTooDear,testBuyHandPremium,
	testSell,testSellTooDear,testSellHandPremium,
	testExchange,testSellNoBag]

testEmpty=TestLabel "Test Empty" (TestCase (do
	let jp=createTestChar "JP"
	let merchant=createTestChar "Merchant"
	assertEqual "Non empty Buy" 0 (length $ listTradeItems jp merchant Buy (Success Standard 1))
	assertEqual "Non empty Sell" 0 (length $ listTradeItems jp merchant Sell (Success Standard 1))
	assertEqual "Non empty Exchange" 0 (length $ listTradeItems jp merchant Exchange (Success Standard 1))
	))

testBuy=TestLabel "Test Buy" (TestCase (do
	let jp=(createTestChar "JP"){inventory=(makeEmptyInventory 10 10)}
	let Right (i,_)=takeItem mkEmptyInventory sword (Bag 1)
	let merchant=(createTestChar "Merchant"){inventory=i}
	let buyops=listTradeItems jp merchant Buy (Success Standard 1)
	assertEqual "Non single Buy" 1 (length buyops)
	assertEqual "Non equal to sword" (ToBuy (Bag 1,sword) 10) (head buyops)
	let (jp2,merchant2,dropped)=doTradeOperation (jp,Just (Bag 1)) (merchant,Nothing) (head buyops)
	assertEqual "merchant gold not equals to 10" 10 (getGold $ inventory merchant2)
	assertEqual "JP does not have the sword!" (Bag 1,sword) (head $ listCarriedItems $ inventory jp2)
	assertEqual "Dropped is not null" 0 (length dropped)
	))	
	
testBuyTooDear=TestLabel "Test Buy Too Dear" (TestCase (do
	let jp=(createTestChar "JP"){inventory=(makeEmptyInventory 10 1)}
	let Right (i,_)=takeItem mkEmptyInventory sword (Bag 1)
	let merchant=(createTestChar "Merchant"){inventory=i}
	assertEqual "Non empty Buy" 0 (length $ listTradeItems jp merchant Buy (Success Standard 1))
	))	
	
testBuyHandPremium=TestLabel "Test Buy Hand Premium" (TestCase (do
	let jp=(createTestChar "JP"){inventory=(makeEmptyInventory 10 20)}
	let Right (i,_)=takeItem mkEmptyInventory sword RightHand
	let merchant=(createTestChar "Merchant"){inventory=i}
	let buyops=listTradeItems jp merchant Buy (Success Standard 1)
	assertEqual "Non single Buy" 1 (length buyops)
	assertEqual "Non equal to sword" (ToBuy (RightHand,sword) 14) (head buyops)
	))	

testSell=TestLabel "Test Sell" (TestCase (do
	let Right (i,_)=takeItem mkEmptyInventory sword (Bag 1)
	let jp=(createTestChar "JP"){inventory=i}
	let merchant=(createTestChar "Merchant"){inventory=(makeEmptyInventory 10 10)}
	let sellops=listTradeItems jp merchant Sell (Success Standard 1)
	assertEqual "Non single Sell" 1 (length sellops)
	assertEqual "Non equal to sword" (ToSell (Bag 1,sword) 10) (head sellops)
	))	
	
testSellTooDear=TestLabel "Test Sell Too Dear" (TestCase (do
	let Right (i,_)=takeItem mkEmptyInventory sword (Bag 1)
	let jp=(createTestChar "JP"){inventory=i}
	let merchant=(createTestChar "Merchant"){inventory=(makeEmptyInventory 10 1)}
	assertEqual "Non empty Sell" 0 (length $ listTradeItems jp merchant Sell (Success Standard 1))
	))		
	
testSellHandPremium=TestLabel "Test Sell Hand Premium" (TestCase (do
	let Right (i,_)=takeItem mkEmptyInventory sword RightHand
	let jp=(createTestChar "JP"){inventory=i}
	let merchant=(createTestChar "Merchant"){inventory=(makeEmptyInventory 10 20)}
	let sellops=listTradeItems jp merchant Sell (Success Standard 1)
	assertEqual "Non single Sell" 1 (length sellops)
	assertEqual "Non equal to sword" (ToSell (RightHand,sword) 14) (head sellops)
	))	
	
testExchange=TestLabel "Test Exchange" (TestCase (do
	let Right (i,_)=takeItem mkEmptyInventory sword (Bag 1)
	let jp=(createTestChar "JP"){inventory=i}
	let Right (i2,_)=takeItem mkEmptyInventory sword (Bag 2)
	let merchant=(createTestChar "Merchant"){inventory=i2}
	let excops=listTradeItems jp merchant Exchange (Success Standard 1)
	assertEqual "Non single Exchange" 1 (length excops)
	assertEqual "Non equal to sword" (ToExchange (Bag 1,sword) (Bag 2,sword) ) (head excops)
	let Right (ijp2,[])=takeItem i dagger (Bag 2)
	let Right (im2,[])=takeItem i2 hatchet (Bag 3)
	assertEqual "getNpcPosition" (Just RightHand) (getNpcPosition (head excops) im2)
	let (jp2,merchant2,dropped)=doTradeOperation (jp{inventory=ijp2},Just (Bag 2)) (merchant{inventory=im2},Just (Bag 3)) (head excops)
	assertEqual "JP2 does not have the sword!" (Bag 2,sword) (head $ listCarriedItems $ inventory jp2)
	assertEqual "Merchant does not have the sword!" (Bag 3,sword) (head $ listCarriedItems $ inventory merchant2)
	assertEqual "dropped is not 2" 2 (length dropped)
	assertBool "dropped does not contain dagger" (elem dagger dropped)
	assertBool "dropped does not contain hatchet" (elem hatchet dropped)
	))
	
testSellNoBag=TestLabel "Test Sell No Bag" (TestCase (do
        let Right (i,_)=takeItem mkEmptyInventory minorHealingPotion (Bag 1)
        let jp=(createTestChar "JP"){inventory=i}
        let ig=makeEmptyInventory 0 100
        let ghoul=(createTestChar "Ghoul"){inventory=ig}
        assertEqual "Has allowed positions" [] (listAllowedPositions ig minorHealingPotion)
        let excops=listTradeItems jp ghoul Sell (Success Standard 1)
        assertEqual "Has a Sell" 0 (length excops)
        ))