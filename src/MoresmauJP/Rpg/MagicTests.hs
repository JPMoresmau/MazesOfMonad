-- | Magic hunit tests
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.MagicTests where

import Control.Monad.Writer

import MoresmauJP.Rpg.Character
import MoresmauJP.Rpg.CharacterTests
import MoresmauJP.Rpg.Magic

import MoresmauJP.Util.Random

import Test.HUnit

magicTests=TestList [testSpellsToMyselfTemporary,testSpellsToMyselfPermanent,testSpellToOpponentSuccess
	,testSpellToOpponentSuccessWeak,testSpellToOpponentFailure,testSpellToOpponentFumble,
	testSpellToOpponentFumbleMad,testSpellToOpponentFumbleIntelligence,testSpellToOpponentFumbleForget,
	testSpellToOpponentFumbleBackFires]

testSpellsToMyselfTemporary= TestLabel "Test spells to myself Temporary" (TestCase (do
	let jp=(createTestChar "jp"){spells=[Spell "Nimble Fingers" Dexterity Positive Temporary]}
	let spellsToMe1=spellsToMyself jp
	assertEqual "not one spell" 1 (length spellsToMe1)
	assertEqual "not nimble fingers" "Nimble Fingers" (spellName $ head spellsToMe1)
	(jp2,msgs)<-evalRandT (runWriterT (spellToMyself jp (head spellsToMe1) 1)) (TestRandom [9])
	assertEqual "not two messages" 2 (length msgs)
	assertEqual "not Nimble Fingers message" "The spell Nimble Fingers worked" (head msgs)
	assertEqual "not under affect message" "Under the influence of Nimble Fingers (1 Dexterity point)" (head $ tail msgs)
	assertEqual "dex is not 11" 11 (getCharacteristic' jp2 Current Dexterity) 
	let spellsToMe2=spellsToMyself jp2
	assertEqual "not zero spell2" 0 (length spellsToMe2)
	let (jp3,_)=expireAffects jp2 4
	let spellsToMe3=spellsToMyself jp3
	assertEqual "not zero spell3" 0 (length spellsToMe3)
	let (jp4,_)=expireAffects jp2 5
	let spellsToMe4=spellsToMyself jp4
	assertEqual "not one spell4" 1 (length spellsToMe4)
	))
	
testSpellsToMyselfPermanent= TestLabel "Test spells to myself Permanent" (TestCase (do
	let jp0=(createTestChar "jp"){spells=[Spell "Feel Better" Physical Recovery Permanent]}
	let jp=setCharacteristic' jp0 Current Physical 9
	let spellsToMe1=spellsToMyself jp
	assertEqual "not one spell" 1 (length spellsToMe1)
	assertEqual "not Feel Better" "Feel Better" (spellName $ head spellsToMe1)
	(jp2,msgs)<-evalRandT (runWriterT (spellToMyself jp (head spellsToMe1) 1)) (TestRandom [9])
	assertEqual "not two messages" 2 (length msgs)
	assertEqual "not Feel Better message" "The spell Feel Better worked" (head msgs)
	assertEqual "not recover message" "You recover 1 Physical point!" (head $ tail msgs)
	assertEqual "physical is not 10" 10 (getCharacteristic' jp2 Current Physical) 
	let spellsToMe2=spellsToMyself jp2
	assertEqual "not one spell" 1 (length spellsToMe2)
	assertEqual "not Feel Better" "Feel Better" (spellName $ head spellsToMe2)
	))	
	
testSpellToOpponentSuccess= TestLabel "Test spell to opponent success" (TestCase (do
	let jp=(createTestChar "JP") {spells=[Spell "Fire Ball" Physical Negative Permanent]}
	let troll=createTestChar "Troll"
	(((_,troll1),isDead),msgs)<-evalRandT (runWriterT (spellToOpponent jp troll (head $ spells jp) 0)) (TestRandom [9])
	assertBool "dead" (not isDead)
	assertEqual "not physical 9" 9 (getCharacteristic' troll1 Current Physical)
	assertEqual "not 1 message" 1 (length msgs)
	assertEqual "not Fire Ball message" "JP casts Fire Ball" (head msgs)
	))
	
testSpellToOpponentSuccessWeak= TestLabel "Test spell to weak opponent success" (TestCase (do
	let jp=(createTestChar "JP") {spells=[Spell "Fire Ball" Physical Negative Permanent]}
	let troll=setCharacteristic' (createTestChar "Troll") Current Willpower 5
	(((_,troll1),isDead),msgs)<-evalRandT (runWriterT (spellToOpponent jp troll (head $ spells jp) 0)) (TestRandom [9])
	assertBool "dead" (not isDead)
	assertEqual "not physical 8" 8 (getCharacteristic' troll1 Current Physical)
	assertEqual "not 1 message" 1 (length msgs)
	assertEqual "not Fire Ball message" "JP casts Fire Ball" (head msgs)
	))	
	
testSpellToOpponentFailure= TestLabel "Test spells to opponent failure" (TestCase (do
	let jp=(createTestChar "JP") {spells=[Spell "Fire Ball" Physical Negative Permanent]}
	let troll=createTestChar "Troll"
	(((jp1,troll1),isDead),msgs)<- evalRandT (runWriterT (spellToOpponent jp troll (head $ spells jp) 0)) (TestRandom [11])
	assertBool "dead" (not isDead)
	assertEqual "not physical 10" 10 (getCharacteristic' troll1 Current Physical)
	assertEqual "not mental 10" 10 (getCharacteristic' jp1 Current Mental)
	assertEqual "not 1 message" 1 (length msgs)
	assertEqual "not Fire Ball message" "The spell Fire Ball fails" ( head msgs)
	))	
	
testSpellToOpponentFumble= TestLabel "Test spells to opponent fumble mental" (TestCase (do
	let jp=(createTestChar "JP") {spells=[Spell "Fire Ball" Physical Negative Permanent]}
	let troll=createTestChar "Troll"
	(((jp1,troll1),isDead),msgs)<- evalRandT (runWriterT(spellToOpponent jp troll (head $ spells jp) 0)) (mkTestWrapper [20,10,1])
	assertBool "dead" (not isDead)
	assertEqual "not physical 10" 10 (getCharacteristic' troll1 Current Physical)
	assertEqual "not 1 message" 1 (length msgs)
	assertEqual "not Fire Ball message" "The spell Fire Ball backfires on you and causes you to lose 1 Mental point!" (head msgs)
	assertEqual "Mental must be 9" 9 (getCharacteristic' jp1 Current Mental)
	))	
	
testSpellToOpponentFumbleMad= TestLabel "Test spells to opponent fumble mad" (TestCase (do
	let jp=(createTestChar "JP") {spells=[Spell "Fire Ball" Physical Negative Permanent]}
	let jp'=setCharacteristic' jp Current Mental 1
	let troll=createTestChar "Troll"
	(((jp1,troll1),isMad),msgs)<- evalRandT (runWriterT(spellToOpponent jp' troll (head $ spells jp') 0)) (mkTestWrapper [20,10,1])
	assertBool "not dead" (isMad)
	assertEqual "not physical 10" 10 (getCharacteristic' troll1 Current Physical)
	assertEqual "not 1 message" 1 (length msgs)
	assertEqual "not Fire Ball message" "The spell Fire Ball backfires on you and causes you to lose 1 Mental point!" (head msgs)
	assertEqual "Mental must be 0" 0 (getCharacteristic' jp1 Current Mental)
	))
	
testSpellToOpponentFumbleIntelligence= TestLabel "Test spells to opponent fumble intelligence" (TestCase (do
	let jp=(createTestChar "JP") {spells=[Spell "Fire Ball" Physical Negative Permanent]}
	let troll=createTestChar "Troll"
	(((jp1,troll1),isMad),msgs)<- evalRandT (runWriterT(spellToOpponent jp troll (head $ spells jp) 0)) (mkTestWrapper [20,10,20,1,20,20])
	assertBool "mad" (not isMad)
	assertEqual "not physical 10" 10 (getCharacteristic' troll1 Current Physical)
	assertEqual "not 1 message" 1 (length msgs)
	assertEqual "not Fire Ball message" "The spell Fire Ball confuses you and causes you to lose 1 Intelligence point!" (head msgs)
	assertEqual "Mental must be 9" 9 (getCharacteristic' jp1 Current Intelligence)
	))		
	
testSpellToOpponentFumbleForget= TestLabel "Test spells to opponent fumble forget" (TestCase (do
	let jp=(createTestChar "JP") {spells=[Spell "Fire Ball" Physical Negative Permanent]}
	let troll=createTestChar "Troll"
	(((jp1,troll1),isMad),msgs)<- evalRandT (runWriterT(spellToOpponent jp troll (head $ spells jp) 0)) (mkTestWrapper [20,1,20,20,20,20,20])
	assertBool "mad" (not isMad)
	assertEqual "not physical 10" 10 (getCharacteristic' troll1 Current Physical)
	assertEqual "not 1 message" 1 (length msgs)
	assertEqual "not Fire Ball message" "The spell Fire Ball backfires and you promptly forget it!" (head msgs)
	assertEqual "Still has spells" 0 (length $ spells jp1)
	))		
	
testSpellToOpponentFumbleBackFires= TestLabel "Test spells to opponent fumble back fires" (TestCase (do
	let jp=(createTestChar "JP") {spells=[Spell "Fire Ball" Physical Negative Permanent]}
	let troll=createTestChar "Troll"
	(((jp1,troll1),outOfOrder),msgs)<- evalRandT (runWriterT(spellToOpponent jp troll (head $ spells jp) 0)) (mkTestWrapper [20,20,20,20,20,20,20])
	assertBool "outOfOrder" (not outOfOrder)
	assertEqual "not physical 10" 10 (getCharacteristic' troll1 Current Physical)
	assertEqual "not 1 message" 1 (length msgs)
	assertEqual "not Fire Ball message" "The spell Fire Ball bounces back and hits you!" (head msgs)
	assertEqual "not physical 2" 2 (getCharacteristic' jp1 Current Physical)
	))		