
module MoresmauJP.Rpg.Stats where

import qualified Data.Map as M
import Data.Maybe

import MoresmauJP.Maze1.Maze

import MoresmauJP.Rpg.Character
import MoresmauJP.Rpg.Items
import MoresmauJP.Rpg.MazeObjects
import MoresmauJP.Rpg.NPC
import MoresmauJP.Rpg.Save

import MoresmauJP.Util.Numbers

import System.Directory
import System.FilePath
import System.IO

data GameStats = GameState {
	charLevel::Int
	,numberOfNPCs::Int
	,proportionNPCs::Int
	,averageNPCLevel::Int
	,averageNPCAttitude::Int
	,numberOfItems::Int
	,proportionItems::Int
	,numberOfTraps::Int
	,proportionTraps::Int
	} deriving (Show,Read)

gameStats :: RPGState -> GameStats
gameStats rs=let
	chLevel=characterLevel (fromJust $ rpgCharacter rs)
	mgw=mazegameworld $ fromJust $ mgs rs
	(h,w)=size $ maze mgw
	cellCount=h*w
	allNPCs=M.elems $ npcs $ objects $ fromJust $ mgs rs
	npcCount=length allNPCs
	npcProp=div cellCount npcCount
	avgAttitude=avg (map npcAttitude allNPCs)
	avgLevel=avg (map (characterLevel . npcCharacter) allNPCs)
	allItems=(filter (not . isTrap)) $ concat $ M.elems $ items $ objects $ fromJust $ mgs rs
	itemCount=length allItems
	itemProp=div cellCount itemCount
	allTraps=(filter isTrap) $ concat $ M.elems $ items $ objects $ fromJust $ mgs rs
	trapCount=length allTraps
	trapProp=div cellCount trapCount		
	in GameState chLevel npcCount npcProp avgLevel avgAttitude itemCount itemProp trapCount trapProp
	
fileGameStats :: String -> IO GameStats
fileGameStats name=do
	dir<-getAppUserDataDirectory "MazesOfMonad"
	let fp=dir</> (makeValid $ (addExtension name backupExtension))
	withFile fp ReadMode (\h -> do
		s<-hGetContents h
		c<-(readIO s)::IO BackupState
		let rs=fromBackupState (RPGState Nothing Nothing (takeDirectory fp)) c
		return (gameStats rs)
		)