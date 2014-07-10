-- | Handling of items in the maze
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.MazeObjects where

import Control.Monad

import Data.List
import Data.Maybe
import qualified Data.Map as M
import MoresmauJP.Maze1.Maze
import MoresmauJP.Rpg.Character
import MoresmauJP.Rpg.Inventory hiding (dropItem)
import MoresmauJP.Rpg.Items
import MoresmauJP.Rpg.Magic
import MoresmauJP.Rpg.NPC
import MoresmauJP.Util.Lists
import MoresmauJP.Util.Random

import Text.Printf

data MazeObjects= MazeObjects {
	items::(M.Map Cell [ItemType]),
	npcs::(M.Map Cell NPCCharacter)
	}
	deriving (Read,Show,Eq)

data MazeOptions = MazeOptions {
		itemProportion::Int,
		npcProportion::Int,
		trapProportion::Int
	}
	deriving (Read,Show)

generateObjects :: (MonadRandom m)=>Maze -> MazeOptions -> Int -> m MazeObjects
generateObjects mz options characterLevel =do
	let ratio=((fromIntegral characterLevel/12.0) ** 2)::Float
	-- less items as we progress
	let realItemProportion=(fromIntegral $ itemProportion options) * ratio
	itemMap<-generateItems (size mz) realItemProportion
	let realTrapProportion=(fromIntegral $ trapProportion options) / ratio
	itemMap2<-generateTraps (size mz) realTrapProportion itemMap
	npcMap<-generateNPCs mz (fromIntegral $ npcProportion options) characterLevel
	return (MazeObjects itemMap2 npcMap)
	
generateItems :: (MonadRandom m)=>Size -> Float -> m (M.Map Cell [ItemType])
generateItems sz proportion=do
	cellsWithItems<-getCellWithItems sz proportion
	let occ=occurenceList itemOccurences
	foldM (\m cell -> do
		it <- generateScroll =<< randomPickp occ
		let m2=case M.lookup cell m of
			Nothing->M.insert cell [it] m
			Just its->M.insert cell (it:its) m
		return m2) M.empty cellsWithItems
		
generateScroll :: (MonadRandom m) => ItemType -> m (ItemType)
generateScroll (Scroll "" "" g)=do
	s<-randomPickp allSpells
	return (Scroll (printf "Scroll of %s" (spellName s)) (spellName s) g)
generateScroll it=return it

generateTraps :: (MonadRandom m)=>Size -> Float -> (M.Map Cell [ItemType]) -> m (M.Map Cell [ItemType])
generateTraps sz proportion m=do
	cellsWithItems<-getCellWithItems sz proportion
	let occ=occurenceList trapOccurences
	foldM (\m cell -> do
		it <- randomPickp occ
		let m2=case M.lookup cell m of
			Nothing->M.insert cell [it] m
			Just its->M.insert cell (it:its) m
		return m2) m cellsWithItems
		

generateNPCs :: (MonadRandom m)=> Maze -> Float -> Int -> m (M.Map Cell NPCCharacter)
generateNPCs mz proportion characterLevel=do
	cellsWithItems<-(getCellWithItems (size mz) proportion) 
	let attitudeModifier=floor ((fromIntegral characterLevel/10.0) ** 2)
	-- no npc on start cell
	let cells2=delete (start mz) cellsWithItems
	let occ=occurenceList (getNPCOccurences characterLevel)
	foldM (\m cell -> do
		t <- randomPickp occ
		case M.lookup cell m of
			Nothing->do
				c<-generateFromTemplate t attitudeModifier
				return (M.insert cell c m)
			Just _->return m
		) M.empty cells2
	
getCellWithItems :: (MonadRandom m)=> Size -> Float -> m [Cell]
getCellWithItems (w,h) proportion=do
	let nb=round (fromIntegral (h*w)/proportion)
	replicateM nb (getCellWithItems' (w,h))
	
getCellWithItems' :: (MonadRandom m)=> Size -> m Cell
getCellWithItems' (w,h) =do
		cellx<-getRandomRange (1,w)
		celly<-getRandomRange (1,h)
		return (cellx,celly)
		
listPickableItems :: GameWorld -> MazeObjects -> [ItemType]
listPickableItems gw mo=(filter (not . isTrap)) $ listItems' gw mo

getTrap:: GameWorld -> MazeObjects -> (MazeObjects,Maybe ItemType)
getTrap gw mo=let
 	maybeTrap=listToMaybe $ (filter isTrap) $ listItems' gw mo
	in case maybeTrap of
		Nothing->(mo,Nothing)
		Just it->((MoresmauJP.Rpg.MazeObjects.takeItem gw mo it),Just it)

listItems':: GameWorld -> MazeObjects -> [ItemType]
listItems' gw mo =fromMaybe [] $ M.lookup (position gw) (items mo)


takeItem :: GameWorld -> MazeObjects -> ItemType -> MazeObjects
takeItem gw mo it= 
	let its=fromJust $ M.lookup (position gw) (items mo)
	in mo{items=(M.insert (position gw) (delete it its) (items mo))}

dropItem :: GameWorld -> MazeObjects -> ItemType -> MazeObjects
dropItem gw mo it= 
	let its=case M.lookup (position gw) (items mo) of 
		Just its-> its
		Nothing->[]
	in mo{items= (M.insert (position gw) (it:its) (items mo))}
	
getNPC:: GameWorld -> MazeObjects -> Maybe NPCCharacter
getNPC gw mo = M.lookup (position gw) (npcs mo)
	
killNPC :: GameWorld -> MazeObjects -> Character -> (MazeObjects,Character,Gold)
killNPC gw mo c@(Character {inventory=inv})= let
	killed=fromJust $ M.lookup (position gw) (npcs mo)
	killedItems=map snd (listCarriedItemsUniqueObject $ inventory $ npcCharacter killed)
	mo2=foldl (\mazeobj item->dropItem gw mazeobj item) mo killedItems
	extraGold=getGold $ inventory $ npcCharacter killed
	c2=c{inventory=addGold inv extraGold}
 	in (mo2{npcs= (M.delete (position gw) (npcs mo2))},c2,extraGold)	

updateNPC :: GameWorld -> MazeObjects -> NPCCharacter -> MazeObjects
updateNPC gw mo npc= mo{npcs= (M.insert (position gw) npc (npcs mo))}

moveNPC :: (MonadRandom m)=> GameWorld -> MazeObjects -> NPCCharacter -> m MazeObjects
moveNPC gw mo npc= do
	let mo2=mo{npcs= (M.delete (position gw) (npcs mo))}
	let eligibleCells= filter (\x->x /= (position gw) && M.notMember x (npcs mo2)) (M.keys $ cellmap $ maze $ gw)
	cell<-randomPickp eligibleCells 
	return mo2{npcs= (M.insert cell npc (npcs mo2))}
	
recoverNPCWithTime:: MazeObjects -> Int -> Int -> MazeObjects
recoverNPCWithTime mo tickCount toAdd=let
	allnpcs=npcs mo
	allnpcs2=M.map (\npc@(NPCCharacter{npcCharacter=c})->npc{npcCharacter=restoreWithTime c tickCount toAdd}) allnpcs
	in mo{npcs=allnpcs2}