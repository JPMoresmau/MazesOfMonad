-- | The "arena" hunit tests
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.ArenaTests where

import Control.Monad.State
import Control.Monad.Writer

import MoresmauJP.Core.Screen
import MoresmauJP.Rpg.Arena
import MoresmauJP.Rpg.Character
import MoresmauJP.Rpg.CharacterTests
import MoresmauJP.Rpg.Inventory
import MoresmauJP.Rpg.NPC
import MoresmauJP.Rpg.NPCTests

import MoresmauJP.Util.Random

import Test.HUnit

arenaTests=TestList [testStealFailure,testStealFailureExceptional,testStealSuccess,
	testStealSuccessGold,testStealSuccessGoldItem,testStealSuccessNothing,
	testConvertFailure,testConvertFailureExceptional,testConvertSuccess,
	testBribeSuccess,testBribeSuccessAttitude,testBribeFailure,testBribeFailureExceptional,testBribeNoGold,
	testPrayerSuccess,testPrayerFailure,testPrayerFailureExceptional,
	testPossibleActions]

testStealFailure=TestLabel "Test Steal Failure" (TestCase (do
	let jp=createTestChar "JP"
	let item=Weapon "Sword" 2 6 1 10
	let v1=(createTestNPC "Victim")
	let vc1=(npcCharacter v1){inventory=makeFullInventory [(Bag 1,item)] 10 100}
	let victim=v1{npcCharacter=vc1}
	let arena=Arena jp victim Nothing True [] 0 newFightInfo
	let gs=GameState arena Nothing
	gs2<- execStateT (evalRandT (runWriterT stealInArena) (mkTestWrapper [12])) gs
	let arena2=gsData gs2
	assertEqual "status not finished" (Just Finished) (es arena2) 
	assertEqual "not zero items" 0 (length $ arenaItems arena2)
	assertEqual "not item in victim bag" item (snd $ head $ filter (((Bag 1) ==) . fst) $
		listCarriedItems $ inventory $ npcCharacter $ arenaOpponent arena2)
	assertEqual "attitude is not 8" 8 (npcAttitude $ arenaOpponent arena2)
	assertEqual "gold is not 0" 0 (getGold $ inventory $ arenaCharacter arena2)
	))
	
testStealFailureExceptional=TestLabel "Test Steal Failure Exceptional" (TestCase (do
	let jp=createTestChar "JP"
	let item=Weapon "Sword" 2 6 1 10
	let v1=(createTestNPC "Victim")
	let vc1=(npcCharacter v1){inventory=makeFullInventory [(Bag 1,item)] 10 100}
	let victim=v1{npcCharacter=vc1}
	let arena=Arena jp victim Nothing True [] 0 newFightInfo
	let gs=GameState arena Nothing
	gs2<- execStateT (evalRandT (runWriterT stealInArena) (mkTestWrapper [20])) gs
	let arena2=gsData gs2
	assertEqual "status finished" Nothing (es arena2) 
	assertEqual "not zero items" 0 (length $ arenaItems arena2)
	assertEqual "not item in victim bag" item (snd $ head $ filter (((Bag 1) ==) . fst) $
		listCarriedItems $ inventory $ npcCharacter $ arenaOpponent arena2)
	assertEqual "attitude is not 1" 1 (npcAttitude $ arenaOpponent arena2)
	assertEqual "gold is not 0" 0 (getGold $ inventory $ arenaCharacter arena2)
	))	
	
testStealSuccess=TestLabel "Test Steal Success" (TestCase (do
	let jp=createTestChar "JP"
	let item=Weapon "Sword" 2 6 1 10
	let v1=(createTestNPC "Victim")
	let vc1=(npcCharacter v1){inventory=makeFullInventory [(Bag 1,item)] 10 100}
	let victim=v1{npcCharacter=vc1}
	let arena=Arena jp victim Nothing True [] 0 newFightInfo
	let gs=GameState arena Nothing
	gs2<- execStateT (evalRandT (runWriterT stealInArena) (mkTestWrapper [8])) gs
	let arena2=gsData gs2
	assertEqual "status not finished" (Just Finished) (es arena2) 
	assertEqual "not one item" 1 (length $ arenaItems arena2)
	assertEqual "not sword" item (head $ arenaItems arena2)
	assertEqual "item in victim bag" 0 (length $ filter (((Bag 1) ==) . fst) $
		listCarriedItems $ inventory $ npcCharacter $ arenaOpponent arena2)
	assertEqual "attitude is not 10" 10 (npcAttitude $ arenaOpponent arena2)
	assertEqual "gold is not 0" 0 (getGold $ inventory $ arenaCharacter arena2)
	))
	
testStealSuccessGold=TestLabel "Test Steal Success Gold" (TestCase (do
	let jp=createTestChar "JP"
	let v1=(createTestNPC "Victim")
	let vc1=(npcCharacter v1){inventory=makeFullInventory [] 10 100}
	let victim=v1{npcCharacter=vc1}
	let arena=Arena jp victim Nothing True [] 0 newFightInfo
	let gs=GameState arena Nothing
	gs2<- execStateT (evalRandT (runWriterT stealInArena) (mkTestWrapper [8])) gs
	let arena2=gsData gs2
	assertEqual "status not finished" (Just Finished) (es arena2) 
	assertEqual "not zero item" 0 (length $ arenaItems arena2)
	assertEqual "attitude is not 10" 10 (npcAttitude $ arenaOpponent arena2)
	assertEqual "jp gold is not 5" 5 (getGold $ inventory $ arenaCharacter arena2)
	assertEqual "victim gold is not 95" 95 (getGold $ inventory $ npcCharacter $ arenaOpponent arena2)
	))	
	
testStealSuccessGoldItem=TestLabel "Test Steal Success Gold+Item" (TestCase (do
	let jp=createTestChar "JP"
	let item=Weapon "Sword" 2 6 1 10
	let v1=(createTestNPC "Victim")
	let vc1=(npcCharacter v1){inventory=makeFullInventory [(Bag 1,item)] 10 100}
	let victim=v1{npcCharacter=vc1}
	let arena=Arena jp victim Nothing True [] 0 newFightInfo
	let gs=GameState arena Nothing
	gs2<- execStateT (evalRandT (runWriterT stealInArena) (mkTestWrapper [4])) gs
	let arena2=gsData gs2
	assertEqual "status not finished" (Just Finished) (es arena2) 
	assertEqual "not one item" 1 (length $ arenaItems arena2)
	assertEqual "not sword" item (head $ arenaItems arena2)
	assertEqual "item in victim bag" 0 (length $ filter (((Bag 1) ==) . fst) $
		listCarriedItems $ inventory $ npcCharacter $ arenaOpponent arena2)
	assertEqual "attitude is not 10" 10 (npcAttitude $ arenaOpponent arena2)
	assertEqual "jp gold is not 18" 18 (getGold $ inventory $ arenaCharacter arena2)
	assertEqual "victim gold is not 82" 82 (getGold $ inventory $ npcCharacter $ arenaOpponent arena2)
	
	))	
	
testStealSuccessNothing=TestLabel "Test Steal Success Nothing" (TestCase (do
	let jp=createTestChar "JP"
	let v1=(createTestNPC "Victim")
	let vc1=(npcCharacter v1){inventory=makeFullInventory [] 10 0}
	let victim=v1{npcCharacter=vc1}
	let arena=Arena jp victim Nothing True [] 0 newFightInfo
	let gs=GameState arena Nothing
	gs2<- execStateT (evalRandT (runWriterT stealInArena) (mkTestWrapper [8])) gs
	let arena2=gsData gs2
	assertEqual "status not finished" (Just Finished) (es arena2) 
	assertEqual "not zero item" 0 (length $ arenaItems arena2)
	assertEqual "attitude is not 10" 10 (npcAttitude $ arenaOpponent arena2)
	assertEqual "gold is not 0" 0 (getGold $ inventory $ arenaCharacter arena2)
	assertEqual "victim gold is not 0" 0 (getGold $ inventory $ npcCharacter $ arenaOpponent arena2)
	))	
	
testConvertFailure=TestLabel "Test Convert Failure" (TestCase (do
	let jp=createTestChar "JP"
	let item=Weapon "Sword" 2 6 1 10
	let v1=(createTestNPC "Victim")
	let vc1=(npcCharacter v1){inventory=makeFullInventory [(Bag 1,item)] 10 100}
	let victim=v1{npcCharacter=vc1}
	let arena=Arena jp victim Nothing True [] 0 newFightInfo
	let gs=GameState arena Nothing
	gs2<- execStateT (evalRandT (runWriterT convertInArena) (mkTestWrapper [12])) gs
	let arena2=gsData gs2
	assertEqual "status not finished" (Just Finished) (es arena2) 
	assertEqual "not zero items" 0 (length $ arenaItems arena2)
	assertEqual "not item in victim bag" item (snd $ head $ filter (((Bag 1) ==) . fst) $
		listCarriedItems $ inventory $ npcCharacter $ arenaOpponent arena2)
	assertEqual "attitude is not 10" 10 (npcAttitude $ arenaOpponent arena2)
	assertEqual "gold is not 0" 0 (getGold $ inventory $ arenaCharacter arena2)	
	))
	
testConvertFailureExceptional=TestLabel "Test Convert Failure Exceptional" (TestCase (do
	let jp=createTestChar "JP"
	let item=Weapon "Sword" 2 6 1 10
	let v1=(createTestNPC "Victim")
	let vc1=(npcCharacter v1){inventory=makeFullInventory [(Bag 1,item)] 10 100}
	let victim=v1{npcCharacter=vc1}
	let arena=Arena jp victim Nothing True [] 0 newFightInfo
	let gs=GameState arena Nothing
	gs2<- execStateT (evalRandT (runWriterT convertInArena) (mkTestWrapper [20])) gs
	let arena2=gsData gs2
	assertEqual "status finished" Nothing (es arena2) 
	assertEqual "not zero items" 0 (length $ arenaItems arena2)
	assertEqual "not item in victim bag" item (snd $ head $ filter (((Bag 1) ==) . fst) $
		listCarriedItems $ inventory $ npcCharacter $ arenaOpponent arena2)
	assertEqual "attitude is not 1" 1 (npcAttitude $ arenaOpponent arena2)
	assertEqual "gold is not 0" 0 (getGold $ inventory $ arenaCharacter arena2)
	))		
	
testConvertSuccess=TestLabel "Test Convert Success" (TestCase (do
	let jp=createTestChar "JP"
	let item=Weapon "Sword" 2 6 1 10
	let v1=(createTestNPC "Victim")
	let vc1=(npcCharacter v1){inventory=makeFullInventory [(Bag 1,item)] 10 100}
	let victim=v1{npcCharacter=vc1}
	let arena=Arena jp victim Nothing True [] 0 newFightInfo
	let gs=GameState arena Nothing
	gs2<- execStateT (evalRandT (runWriterT convertInArena) (mkTestWrapper [8])) gs
	let arena2=gsData gs2
	assertEqual "status not finished" (Just Finished) (es arena2) 
	assertEqual "not one item" 1 (length $ arenaItems arena2)
	assertEqual "not sword" item (head $ arenaItems arena2)
	assertEqual "item in victim bag" 0 (length $ filter (((Bag 1) ==) . fst) $
		listCarriedItems $ inventory $ npcCharacter $ arenaOpponent arena2)
	assertEqual "attitude is not 12" 12 (npcAttitude $ arenaOpponent arena2)	
	assertEqual "gold is not 0" 0 (getGold $ inventory $ arenaCharacter arena2)
	))
	
testBribeSuccess=TestLabel "Test Bribe Success" (TestCase (do
	let jp=(createTestChar "JP"){inventory=makeFullInventory [] 10 100}
	let v1=(createTestNPC "Victim")
	let vc1=(npcCharacter v1){inventory=makeFullInventory [] 10 100}
	let victim=v1{npcCharacter=vc1}
	let arena=Arena jp victim Nothing True [] 0 newFightInfo
	let gs=GameState arena Nothing
	gs2<- execStateT (evalRandT (runWriterT bribeInArena) (mkTestWrapper [8])) gs
	let arena2=gsData gs2
	assertEqual "status not finished" (Just Finished) (es arena2) 
	assertEqual "victim gold is not 196" 196 (getGold $ inventory $ npcCharacter $ arenaOpponent arena2)
	assertEqual "gold is not 4" 4 (getGold $ inventory $ arenaCharacter arena2)
	))

testBribeSuccessAttitude=TestLabel "Test Bribe Success Attitude" (TestCase (do
	let jp=(createTestChar "JP"){inventory=makeFullInventory [] 10 100}
	let v1=(createTestNPC "Victim"){npcAttitude=12}
	let vc1=(npcCharacter v1){inventory=makeFullInventory [] 10 100}
	let victim=v1{npcCharacter=vc1}
	let arena=Arena jp victim Nothing True [] 0 newFightInfo
	let gs=GameState arena Nothing
	gs2<- execStateT (evalRandT (runWriterT bribeInArena) (mkTestWrapper [8])) gs
	let arena2=gsData gs2
	assertEqual "status not finished" (Just Finished) (es arena2) 
	assertEqual "victim gold is not 194" 194 (getGold $ inventory $ npcCharacter $ arenaOpponent arena2)
	assertEqual "gold is not 6" 6 (getGold $ inventory $ arenaCharacter arena2)
	))
	
testBribeFailure=TestLabel "Test Bribe Failure" (TestCase (do
	let jp=(createTestChar "JP"){inventory=makeFullInventory [] 10 100}
	let v1=(createTestNPC "Victim")
	let vc1=(npcCharacter v1){inventory=makeFullInventory [] 10 100}
	let victim=v1{npcCharacter=vc1}
	let arena=Arena jp victim Nothing True [] 0 newFightInfo
	let gs=GameState arena Nothing
	gs2<- execStateT (evalRandT (runWriterT bribeInArena) (mkTestWrapper [12])) gs
	let arena2=gsData gs2
	assertEqual "status finished" Nothing (es arena2) 
	assertEqual "victim gold is not 100" 100 (getGold $ inventory $ npcCharacter $ arenaOpponent arena2)
	assertEqual "gold is not 100" 100 (getGold $ inventory $ arenaCharacter arena2)
	))
	
testBribeNoGold=TestLabel "Test Bribe No Gold" (TestCase (do
	let jp=(createTestChar "JP"){inventory=makeFullInventory [] 10 0}
	let v1=(createTestNPC "Victim")
	let vc1=(npcCharacter v1){inventory=makeFullInventory [] 10 100}
	let victim=v1{npcCharacter=vc1}
	let arena=Arena jp victim Nothing True [] 0 newFightInfo
	let gs=GameState arena Nothing
	gs2<- execStateT (evalRandT (runWriterT bribeInArena) (mkTestWrapper [8])) gs
	let arena2=gsData gs2
	assertEqual "status finished" Nothing (es arena2) 
	assertEqual "victim gold is not 100" 100 (getGold $ inventory $ npcCharacter $ arenaOpponent arena2)
	assertEqual "gold is not 0" 0 (getGold $ inventory $ arenaCharacter arena2)
	))
		
	
testBribeFailureExceptional=TestLabel "Test Bribe Failure Exceptional" (TestCase (do
	let jp=(createTestChar "JP"){inventory=makeFullInventory [] 10 100}
	let v1=(createTestNPC "Victim")
	let vc1=(npcCharacter v1){inventory=makeFullInventory [] 10 100}
	let victim=v1{npcCharacter=vc1}
	let arena=Arena jp victim Nothing True [] 0 newFightInfo
	let gs=GameState arena Nothing
	gs2<- execStateT (evalRandT (runWriterT bribeInArena) (mkTestWrapper [20])) gs
	let arena2=gsData gs2
	assertEqual "status finished" Nothing (es arena2) 
	assertEqual "victim gold is not 100" 100 (getGold $ inventory $ npcCharacter $ arenaOpponent arena2)
	assertEqual "gold is not 100" 100 (getGold $ inventory $ arenaCharacter arena2)
	assertEqual "attitude is not 1" 1 (npcAttitude $ arenaOpponent arena2)
	))	
	
testPrayerSuccess=TestLabel "Test Prayer Success" (TestCase (do
	let jp=(createTestChar "JP")
	let v1=(createTestNPC "Victim")
	let arena=Arena jp v1 Nothing True [] 0 newFightInfo
	let gs=GameState arena Nothing
	gs2<- execStateT (evalRandT (runWriterT prayerInArena) (mkTestWrapper [8])) gs
	let arena2=gsData gs2
	assertEqual "status not finished" (Just Finished) (es arena2) 
	assertEqual "attitude is not 12" 12 (npcAttitude $ arenaOpponent arena2)
	))
	
testPrayerFailure=TestLabel "Test Prayer Failure" (TestCase (do
	let jp=(createTestChar "JP"){inventory=makeFullInventory [] 10 100}
	let v1=(createTestNPC "Victim")
	let vc1=(npcCharacter v1){inventory=makeFullInventory [] 10 100}
	let victim=v1{npcCharacter=vc1}
	let arena=Arena jp victim Nothing True [] 0 newFightInfo
	let gs=GameState arena Nothing
	gs2<- execStateT (evalRandT (runWriterT prayerInArena) (mkTestWrapper [12])) gs
	let arena2=gsData gs2
	assertEqual "status finished" Nothing (es arena2)
	assertEqual "attitude is not 10" 10 (npcAttitude $ arenaOpponent arena2)
	))
	
testPrayerFailureExceptional=TestLabel "Test Prayer Failure Exceptional" (TestCase (do
	let jp=(createTestChar "JP"){inventory=makeFullInventory [] 10 100}
	let v1=(createTestNPC "Victim")
	let vc1=(npcCharacter v1){inventory=makeFullInventory [] 10 100}
	let victim=v1{npcCharacter=vc1}
	let arena=Arena jp victim Nothing True [] 0 newFightInfo
	let gs=GameState arena Nothing
	gs2<- execStateT (evalRandT (runWriterT prayerInArena) (mkTestWrapper [20])) gs
	let arena2=gsData gs2
	assertEqual "status finished" Nothing (es arena2)
	assertEqual "attitude is not 1" 1 (npcAttitude $ arenaOpponent arena2)
	))		
	
testPossibleActions=TestLabel "Test possible action" (TestCase (do
	let jp=(createTestChar "JP"){inventory=makeFullInventory [] 10 0}
	let v1=(createTestNPC "Victim"){npcType=Animal}
	let acts1=getPossibleArenaActions jp v1
	assertEqual (show [Melee,Escape]) [Melee,Escape] acts1
	let jpM1=jp{spells=[Spell "Feel Better" Physical Recovery Permanent]}
	let acts2=getPossibleArenaActions jpM1 v1
	assertEqual (show [Melee,Escape]) [Melee,Escape] acts2
	let jpM2=jp{spells=[Spell "Greasy Fingers" Dexterity Negative Temporary]}
	let acts3=getPossibleArenaActions jpM2 v1
	assertEqual (show [Melee,Escape,Magic]) [Melee,Escape,Magic] acts3	
	let v2=(createTestNPC "Victim"){npcType=Human}
	let acts4=getPossibleArenaActions jpM2 v2
	assertEqual (show [Melee,Escape,Magic,Prayer]) [Melee,Escape,Magic,Prayer] acts4
	let jpG=jpM2{inventory=makeFullInventory [] 10 10}
	let acts5=getPossibleArenaActions jpG v2
	assertEqual (show [Melee,Escape,Magic,Prayer,Bribe]) [Melee,Escape,Magic,Prayer,Bribe] acts5
	
	))