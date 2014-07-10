-- | Items and Mazes hunit tests
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.MazeObjectsTests where

import qualified Data.Map as M

import MoresmauJP.Maze1.Maze
import MoresmauJP.Rpg.Character
import MoresmauJP.Rpg.CharacterTests
import MoresmauJP.Rpg.Inventory
import MoresmauJP.Rpg.MazeObjects
import MoresmauJP.Rpg.NPC
import MoresmauJP.Rpg.NPCTests

import MoresmauJP.Util.Random

import System.Random

import Test.HUnit

mazeObjectsTests=TestList [testScrollFill,testKillNPC,testKillNPC2HandBag,testKillNPC2HandHand]

testScrollFill= TestLabel "Test filling scroll" (TestCase (do
        let sc=Scroll "" "" 5
        sc2<-evalRandT (generateScroll sc) (mkTestWrapper [2])
        assertEqual "not proper scroll" (Scroll "Scroll of Feel Better" "Feel Better" 5) sc2
        ))
        
testKillNPC=TestLabel "Test kill NPC" (TestCase (do
        std<-getStdGen
        gw0<-(evalRandT (generateGameWorld (2,2)) (ProductionRandom std)) 
        
        let 
                gw=gw0{position=(0,0)}
                jp=createTestChar "JP"
                item=Weapon "Sword" 2 6 1 10
                v1=(createTestNPC "Victim")
                vc1=(npcCharacter v1){inventory=makeFullInventory [(Bag 1,item)] 10 100}
                victim=v1{npcCharacter=vc1}
                mz=MazeObjects M.empty (M.fromList [((0,0),victim)])
                (mz2,jp2,g)=killNPC gw mz jp
                dropped=M.lookup (0,0) (items mz2)
        assertEqual "Gold in JP's pocket is not 100" 100 (getGold $ inventory jp2)
        assertEqual "Gold message is not 100" 100 g
        assertEqual "item not dropped" (Just [item]) dropped
        ))
        
testKillNPC2HandBag=TestLabel "Test kill NPC 2 Handed weapon in Bag" (TestCase (do
        std<-getStdGen
        gw0<-(evalRandT (generateGameWorld (2,2)) (ProductionRandom std)) 
        
        let 
                gw=gw0{position=(0,0)}
                jp=createTestChar "JP"
                item=Weapon "2 Handed Sword" 4 12 2 20
                v1=(createTestNPC "Victim")
                vc1=(npcCharacter v1){inventory=makeFullInventory [(Bag 1,item)] 10 100}
                victim=v1{npcCharacter=vc1}
                mz=MazeObjects M.empty (M.fromList [((0,0),victim)])
                (mz2,jp2,g)=killNPC gw mz jp
                dropped=M.lookup (0,0) (items mz2)
        assertEqual "Gold in JP's pocket is not 100" 100 (getGold $ inventory jp2)
        assertEqual "Gold message is not 100" 100 g
        assertEqual "item not dropped" (Just [item]) dropped
        ))        
        
testKillNPC2HandHand=TestLabel "Test kill NPC 2 Handed weapon in Hand" (TestCase (do
        std<-getStdGen
        gw0<-(evalRandT (generateGameWorld (2,2)) (ProductionRandom std)) 
        
        let 
                gw=gw0{position=(0,0)}
                jp=createTestChar "JP"
                item=Weapon "2 Handed Sword" 4 12 2 20
                v1=(createTestNPC "Victim")
                inv1=makeFullInventory [] 10 100
                Right (inv2,_)=MoresmauJP.Rpg.Inventory.takeItem inv1 item RightHand
                vc1=(npcCharacter v1){inventory=inv2}
                victim=v1{npcCharacter=vc1}
                mz=MazeObjects M.empty (M.fromList [((0,0),victim)])
                (mz2,jp2,g)=killNPC gw mz jp
                dropped=M.lookup (0,0) (items mz2)
        assertEqual "Gold in JP's pocket is not 100" 100 (getGold $ inventory jp2)
        assertEqual "Gold message is not 100" 100 g
        assertEqual "item not dropped" (Just [item]) dropped
        ))        