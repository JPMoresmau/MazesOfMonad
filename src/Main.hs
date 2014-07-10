-- | This is the entry point of the application
-- (c) JP Moresmau 2009
module Main where

import MoresmauJP.Core.Screen
import MoresmauJP.Rpg.ActionsTests
import MoresmauJP.Rpg.RPG
import MoresmauJP.Rpg.CharacterTests
import MoresmauJP.Rpg.FightTests
import MoresmauJP.Rpg.ItemsTests
import MoresmauJP.Rpg.InventoryTests
import MoresmauJP.Rpg.MagicTests
import MoresmauJP.Rpg.MazeObjectsTests
import MoresmauJP.Rpg.ProfileTests
import MoresmauJP.Rpg.NPCTests
import MoresmauJP.Rpg.TradeTests
import MoresmauJP.Rpg.SaveTests
import MoresmauJP.Util.ListsTests
import MoresmauJP.Util.NumbersTests
import MoresmauJP.Util.RandomTests
import MoresmauJP.Rpg.ArenaTests
import MoresmauJP.Rpg.Stats

import Test.HUnit (runTestTT,Test(TestList),Counts(..)) 

-- | main entry point
main :: IO()
main = do
	gs<-initialGameStateInApp
	start (WList ["Welcome to Mazes of Monad, JP Moresmau's Role-Playing Game","If you don't know what to type, try help!"], gs)
	return ()

-- | statistics on a game
stats :: FilePath -> IO(GameStats)
stats fp=fileGameStats fp

-- | run unit tests
test :: IO Test.HUnit.Counts
test = do
	runTestTT (concatTestList [
		listsTests,
		numbersTests,
		randomTests,
		characterTests,
		fightTests,
		inventoryTests,
		profileTests,
		tradeTests,
		saveTests,
		actionsTests,
		npcTests,
		itemTests,
		magicTests,
		mazeObjectsTests,
		arenaTests])

concatTestList tl = TestList (concat (map (\(TestList ts) -> ts ) tl))	