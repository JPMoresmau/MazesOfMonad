-- | Handling of main actions (character management, game management, in game actions)
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.RPG where

import Control.Monad.State
import Control.Monad.Writer

import Data.Char
import Data.List
import Data.Maybe

import MoresmauJP.Core.Screen
import MoresmauJP.Maze1.Maze
import qualified MoresmauJP.Maze1.MazeGame as MG

import MoresmauJP.Rpg.Actions
import MoresmauJP.Rpg.Arena
import MoresmauJP.Rpg.Character
import MoresmauJP.Rpg.Fight
import MoresmauJP.Rpg.Items
import MoresmauJP.Rpg.NPC
import MoresmauJP.Rpg.Profile
import MoresmauJP.Rpg.TextOutput
import MoresmauJP.Rpg.Inventory as Inv
import MoresmauJP.Rpg.Magic
import MoresmauJP.Rpg.MazeObjects as Objs
import MoresmauJP.Rpg.Save
import MoresmauJP.Util.Lists
import MoresmauJP.Util.Random
import System.Directory

import Text.Printf

type LocationCode=String

initialGameStateInApp :: IO(GameState RPGState)
initialGameStateInApp = do
	dir<-getAppUserDataDirectory "MazesOfMonad"
	--createDirectoryIfMissing True dir
	return $ initialGameState dir

initialGameState :: FilePath -> GameState RPGState
initialGameState fp=GameState (RPGState Nothing Nothing fp)  (Just initialScreen)

initialScreen :: Screen RPGState 
initialScreen = Screen [Action "games" "Games: new game, load , delete" games,
	Action "characters" "Characters: new character, delete" chars
	]
	
games:: ActionFunction RPGState 
games _=do
	sc <- gets (fromJust . screen)
	a <- gets gsData 
	put (GameState a (Just (getGameScreen sc)))
	w2 <- help False []
	return (combineWidget  (WText "Games") w2)
	--combineActionAfter (WText "Games",) (help False) (Message [])
	
chars:: ActionFunction RPGState 
chars _  = do
	sc <- gets (fromJust . screen)
	a <- gets gsData 
	put (GameState a (Just (getCharacterScreen sc)))
	w2 <- help False []
	return (combineWidget  (WText "Characters") w2)

--combineActionAfter (WText "Characters",(GameState a (Just (getCharacterScreen sc)))) (help False) (Message [])

getCharacterScreen ::  Screen RPGState  -> Screen RPGState 
getCharacterScreen sc= Screen [Action "new" "Create a new character" newCharacter,
	Action "delete" "Delete an existing character" deleteCharacters,
	Action "view" "View an existing character" viewCharacters,
	backAction sc
	]

newCharacter :: ActionFunction RPGState 
newCharacter _ =return (WInput ["Enter name:"] nameF)

nameF :: String -> WScreenT RPGState 
nameF "" =return (WText "Creation canceled")
nameF name =do
	gs <- get
	exists<-liftIO $ doesCharacterExists (gsData gs) name 
	if exists
		then
			return (WInput [name++" already exists!","Enter name:"] nameF)
		else
			return ((WCombo ["Choose gender:"] (map show [Male,Female]) (genderF name)))

genderF :: String -> String -> WScreenT RPGState 
genderF _ ""=return (WText "Creation canceled")
genderF name genderName= do
	let gender=read (genderName)
	return ((getMappedCombo fst ["Choose profile:"] profiles (profile (name,gender))))

profile :: (String,Gender) -> ComboResult Profile -> WScreenT RPGState 
profile _ Empty=return (WText "Creation canceled")
profile (name,gender) (Unknown _)= do
	w2<- (genderF name) (show gender)
	return (combineWidget (WText "Unknown profile") w2)
profile (name,gender) (Exact prof)= do
	c<-generateCharacter name gender prof
	return (WCheck [ppCharacterAndGold' c] "Accept?" True (profileAccept c))

profileAccept :: Character -> Bool -> WScreenT RPGState 
profileAccept c False = (genderF (name c) (show $ gender c))
profileAccept c True = do
	gs <- get
	s <- liftIO $ saveCharacter (gsData gs) c
	return $ either WText WText s

deleteCharacters :: ActionFunction RPGState 
deleteCharacters _ = listCharactersW $ deleteFileW deleteCharacter

viewCharacters :: ActionFunction RPGState 
viewCharacters _=listCharactersW viewCharacter

listCharactersW :: (String -> WScreenT RPGState) -> WScreenT RPGState
listCharactersW af= do
	rs<-gets gsData
	cs<-liftIO $ listCharacters rs
	if null cs
		then return (WText "No character defined")
	 	else return (WCombo ["Choose character:"] cs (af))	
	--listFilesW (characterExtension,"Choose character:","No character defined") 

{--		
listFilesW::  (String,String,String) -> (String -> WScreenT RPGState) -> WScreenT RPGState
listFilesW (ext,title,msg) af = do
	rs<-gets gsData
	files <- liftIO $ listFiles rs ext
	return 
		(if null files
			then
				(WText msg)
			else
				(WCombo [title] files (af))	
		)		
--}
		
viewCharacter :: String -> WScreenT RPGState 
viewCharacter ""=return (WText "View canceled")
viewCharacter shortName=do
	rs<-gets gsData
	w<-withCharacter rs shortName 
		(\c ->	return $ WText $ ppCharacterAndInventory' c)
	onError w

{--withFileContents :: String -> (String -> IO(Either String a)) -> (a->WScreenT RPGState) -> WScreenT RPGState
withFileContents shortName act act2= do
	rs<-gets gsData
	r<-readF rs shortName act act2
	onError r--}

onError :: Either String (Widget a) -> WScreenT a
onError (Right w)=return w
onError (Left s)=return $ WText s

getGameScreen ::  Screen RPGState  -> Screen RPGState 
getGameScreen sc= Screen [Action "new" "Start a new game" newGame,
	Action "delete" "Delete an existing game" deleteGames,
	Action "play" "Continue playing a game" playGames,
	backAction sc
	]
	
newGame :: ActionFunction RPGState 
newGame _=listCharactersW gameNameF
	
gameNameF :: String -> WScreenT RPGState 
gameNameF ""=return (WText "Creation canceled")
gameNameF shortName=do
	gs <- get
	let rs = gsData gs
	exists<-liftIO $ doesCurrentGameExists rs shortName
	if exists
		then
			return (WText (printf "%s already plays!" shortName))
		else
			do
				r <- withCharacter rs shortName createNewMazeState
				onError r 

mazeOptions :: MazeOptions
mazeOptions = MazeOptions {
	itemProportion=4,
	npcProportion=3,
	trapProportion=10
}


createNewMazeState	::	Character -> WScreenT RPGState	
createNewMazeState c = do
	(GameState gw _) <- MG.initialGameState
	mo<-generateObjects (maze $ gw) mazeOptions (characterLevel c)
	let mgs2=RPGGameState gw mo 0
	createMazeState' c mgs2
		--(printf "Game saved to %s " fileName)
		
createMazeState' :: Character -> RPGGameState -> WScreenT RPGState
createMazeState' c rgs = do
	(RPGState {fp=fp}) <- gets gsData
	let rpg2=RPGState (Just c) (Just rgs) fp
	let ms=getMazeScreen rpg2
	let gs2=GameState rpg2 ms 
	put gs2
	w<-mapAction []
	saveGameAndCharacter w

getMazeScreen :: RPGState  -> Maybe (Screen RPGState)
getMazeScreen RPGState{rpgCharacter=Nothing}  = Nothing
getMazeScreen RPGState{mgs=Nothing}  = Nothing
getMazeScreen rs | isOutOfService $ fromJust $ rpgCharacter rs = Nothing
getMazeScreen RPGState{rpgCharacter=Just c1,mgs=Just mgs}  = 
	let	
		gw=mazegameworld mgs
		dirs=MG.getDirections gw
		f dir = Action (map toLower (show dir)) ("Move " ++ (show dir)) (move dir)
		actions=map f dirs
		mapA= Action "map" "See the map" mapAction
		invA= Action "inventory" "See what you're carrying" inventoryAction
		statusA=Action "character" "See your current character characteristics" statusAction
		bckA= Action "archive" "Save a backup of your game and character" saveBackupAction
		acts=bckA:statusA:mapA:invA:(backAction $ getGameScreen initialScreen):actions
		items=Objs.listPickableItems (mazegameworld mgs) (objects mgs)
		acts2=if null items
			then acts
			else (Action "pickup" "Pick up an item" (pickupAction items)):acts
		itemsCarried=Inv.listCarriedItems (inventory $ c1)
		acts3=if null itemsCarried
			then acts2
			else (Action "drop" "Drop an item" (dropAction itemsCarried)):acts2
		usableItems=filter (canUseItem . snd) itemsCarried
		acts4=if null usableItems
			then acts3
			else (Action "use" "Use an item (drink a potion, read a scroll, etc.)" (useItem usableItems)):acts3
		usableSpells=spellsToMyself c1
		acts5=if null usableSpells
			then acts4
			else (Action "cast" "Cast a spell" (castSpell usableSpells)):acts4
	in	
		Just (Screen acts5)

statusAction :: ActionFunction RPGState
statusAction _ = do
	c<-gets (fromJust . rpgCharacter . gsData)
	return (WText $ ppCharacter' c)

getItems :: RPGState -> Widget RPGState
getItems (RPGState {mgs=Nothing})= WNothing
getItems (RPGState {mgs=Just mgs})=	
	let 
		items=Objs.listPickableItems (mazegameworld mgs) (objects mgs)
	in WList ("You see:": 
			if null items 
				then ["Nothing"]
				else zipWith (\x y->(show x) ++ ": "++(itName y)) [1..] items)

getNPCW :: RPGState -> Widget RPGState
getNPCW (RPGState {mgs=Nothing})= WNothing
getNPCW (RPGState{mgs=Just mgs})=	
	let 
		npc=Objs.getNPC (mazegameworld mgs) (objects mgs)
	in WText ("You meet: "++ 
			(if isNothing npc 
				then "nobody"
				else ("a " ++ (name $ npcCharacter $ fromJust npc))))

move :: MG.Direction -> ActionFunction RPGState
move dir _ =do
	rs@(RPGState {rpgCharacter=Just c1,mgs=(Just rmgs),fp=fp}) <- gets gsData
	let gw=mazegameworld rmgs
	sg <- getSplit
	((WList s,_),(GameState gw2 sc))<- liftIO $ runStateT (evalRandT (runWriterT $ MG.move dir []) sg) (GameState gw Nothing)
	if (isJust sc) 
		then
			do
				rs2<- tick 10 rs{mgs=Just rmgs{mazegameworld=gw2}}
				let (objects2,trap)=Objs.getTrap (gw2) (objects rmgs)
				case trap of
					Nothing -> checkFight rs2 [] s
					Just trap-> do
						let rs3=rs2{mgs=Just (fromJust $ mgs rs2){objects=objects2}}
						(c2,rr)<-action c1 detectTrap (toIntLevel RatherHard)
						case rr of 
							Success {}-> do
								checkFight rs3 [(printf "You avoid a %s" (itName trap))] s							
							Failure {grade=gr}-> do
								--addScreenMessage (triggerDescription trap)
								let s2=[(triggerDescription trap)]
								dmg<-roll (damageLow trap, damageHigh trap)
								(c3,armor)<-damageArmor c2 gr
								let diff=max 0 (dmg-armor)
								if (diff>0) 
									then do
										let s3=s2++[(printf "You lose %s!" (points Physical diff))]
										--addScreenMessage (printf "You lose %s!" (points Physical diff))
										let c4=addCharacteristic' c3 Current Physical (-diff)
										let rs4=rs3{rpgCharacter=Just c4}
										let ms=getMazeScreen rs4
										if isJust ms
											then checkFight rs4 s3 s
											else do
												let gs2=GameState rs4 ms
												put gs2
												saveGameAndCharacter (WList  (s3 ++ ["Game over"]))
									else 
										checkFight rs3 (s2 ++ ["Your armor protects you."]) s
		else
			do
				let nameC=name $ c1
				liftIO $ deleteGame rs nameC
				put (GameState (RPGState Nothing Nothing fp) (Just (getGameScreen initialScreen)))
				return (WList s)	

checkFight :: RPGState -> [String] -> [String] -> WScreenT RPGState
checkFight rs2 pre post= do
	srw<-getSplit
	(rs3,wGold)<-liftIO $ processFight rs2 pre srw
	let ms=getMazeScreen rs3
	let gs2=GameState rs3 ms 
	let (WList itemNames)=getItems rs3
	put gs2
	w<-if isJust ms
			-- we have escaped, probably
			then if (position $ mazegameworld $ fromJust $ mgs rs3)== (position $ mazegameworld $ fromJust $ mgs rs2)
				then return (WList (post++itemNames))
				else mapAction []
			else do
				return (WText "Game over")
	saveGameAndCharacter (combineWidget wGold w)

processFight :: RPGState -> [String] -> RandomWrapper -> IO (RPGState,Widget RPGState)
processFight rs@(RPGState{rpgCharacter=Nothing}) _ _=return (rs,WNothing)
processFight rs@(RPGState{mgs=Nothing}) _ _ =return (rs,WNothing)
processFight rs@(RPGState{rpgCharacter=Just c,mgs=Just mgs}) pre rw= do
	let npc=Objs.getNPC (mazegameworld mgs) (objects mgs)
	if (isJust npc)
		then do
			let npc'=fromJust npc
			let (rw1,rw2)=splitWrapper rw
			es <- screenInteract c npc' pre rw1 (tickCount mgs)
			(mo,c2,extraGold) <- case (exitStatus es) of
				Victory -> return $ killNPC (mazegameworld mgs) (objects mgs) (exitCharacter es)
				EnnemyFlight -> do
					mo2<-evalRandT (moveNPC (mazegameworld mgs) (objects mgs) (exitOpponent es)) rw2
					return (mo2,exitCharacter es,0)
				_ -> return $ (updateNPC (mazegameworld mgs) (objects mgs) (exitOpponent es),exitCharacter es,0)
			let mo2=foldl (Objs.dropItem (mazegameworld mgs)) mo (newItems es)
			gw<-case (exitStatus es) of
				Flight -> do
					let gw=mazegameworld mgs
					evalRandT (randomMove gw) rw2
				_ -> return (mazegameworld mgs)
			let mgs2=mgs{mazegameworld=gw,objects=mo2,tickCount=exitTickCount es}
			let rs2=rs{rpgCharacter=Just c2,mgs=Just mgs2}
			let w=if extraGold>0
				then WText (printf "You pick up %d gold coins" extraGold)
				else WNothing
			return (rs2,w)
		else
			return (rs,WList pre)



mapAction :: ActionFunction RPGState
mapAction _ = do
	rs@(RPGState {mgs=(Just mgs)}) <- gets gsData
	let gw=mazegameworld mgs
	sg <- getSplit
	((WList l,_),_)<-liftIO $ runStateT (evalRandT (runWriterT $ MG.mapAction []) sg) (GameState gw Nothing)
	let itemNames=getItems rs
	return	(combineWidget (WList l) itemNames)
		
	
inventoryAction :: ActionFunction RPGState
inventoryAction _ =do
	(RPGState {rpgCharacter=(Just c)}) <- gets gsData
	return (WText (ppInventory' $ inventory c))

pickupAction :: [ItemType] -> ActionFunction RPGState
pickupAction items cmds = do 
	if null items
		then return (WText "Nothing to pick up")
		else getPretypedWidget (getMappedCombo itName ["Which item?"] items pickupItem) (if null cmds then [] else (tail cmds))

pickupItem :: ComboResult ItemType -> WScreenT RPGState
pickupItem Empty=return (WText "Pick up canceled")
pickupItem (Unknown _)=return (WText "That's not there to pick")
pickupItem (Exact item) =do
	(RPGState {rpgCharacter=(Just c1)}) <- gets gsData
	let positions=Inv.listAllowedPositions (inventory $ c1) item
	return (WCombo ["Put it where?"] (map ppInventoryPosition' positions) (doPickup item positions))

doPickup :: ItemType -> [(Position,Maybe ItemType)] -> String -> WScreenT RPGState
doPickup _ _ ""=return (WText "Pick up canceled")
doPickup item positions pos =do
	(GameState {gsData=rs@(RPGState {rpgCharacter=Just c1,mgs=(Just mgs)})}) <- get
	let matching=filter (\x->(ppInventoryPosition' x)==pos) positions
	if null matching
		then return (WText "You can't put that in there")
		else do 
			let match=head matching
			let Right (inv,it)=Inv.takeItem (inventory $ c1) item (fst match)
			let c2=c1{inventory=inv}
			let objs2=Objs.takeItem (mazegameworld mgs) (objects mgs) item
			let objs3= foldl (\x y->Objs.dropItem (mazegameworld mgs) x y) objs2 it
			let rs2=rs{rpgCharacter=Just c2,mgs=Just mgs{objects=objs3}}
			saveNewGameState rs2
			let itemNames=getItems rs2	
			let w =(combineWidget (WText "Item picked up") itemNames)
			saveGameAndCharacter w

saveNewGameState :: RPGState -> WriterT ScreenMessages (RandT (StateT (GameState RPGState) IO)) ()
saveNewGameState rs2=do
	put (GameState rs2 (getMazeScreen rs2))

saveBackupAction :: ActionFunction RPGState
saveBackupAction _ = do
	rs <- gets gsData
	saveResult<- liftIO $ saveBackup rs			
	return (case saveResult of
		Right (t,_) -> WText t
		Left t -> WText t)

tick :: Int -> RPGState ->  WriterT ScreenMessages (RandT (StateT (GameState RPGState) IO)) (RPGState)
tick _ rs@(RPGState{mgs=Nothing})=return rs
tick tc rs@(RPGState{mgs=Just mgs})=do
	let 
		Just c=rpgCharacter rs
		currentTc=tickCount mgs
		newTick=currentTc+tc
		(c2,s)=expireAffects (restoreWithTime c currentTc tc) newTick
		mo2=recoverNPCWithTime (objects mgs) currentTc tc
		rs2=rs{rpgCharacter=Just c2,mgs=(Just mgs{objects=mo2,tickCount=newTick})}
	mapM addScreenMessage s
	saveNewGameState rs2
	return rs2

dropAction :: [(Position,ItemType)] -> ActionFunction RPGState
dropAction  items _ =do
	return (if null items
		then (WText "You're not carrying anything")
		else (getMappedCombo ppItemPosition' ["What do you want to drop?"] items dropItemAction)
		)
	
dropItemAction ::  ComboResult (Position,ItemType) -> WScreenT RPGState
dropItemAction Empty=return (WText "Drop canceled")
dropItemAction (Unknown _)=return (WText "You can't drop that")
dropItemAction (Exact item) =do
	rs@(RPGState {rpgCharacter=(Just c1),mgs=(Just mgs)}) <- gets gsData
	let Right (inv,it)=Inv.dropItem (inventory $ c1) (fst item)
	let c2=c1{inventory=inv}
	let objs=if isJust it
		then Objs.dropItem (mazegameworld mgs) (objects mgs) (fromJust it)
		else (objects mgs)
	let rs2=rs{rpgCharacter=Just c2,mgs=Just mgs{objects=objs}}
	saveNewGameState rs2
	let itemNames=getItems rs2	
	saveGameAndCharacter (combineWidget (WText "Item dropped") itemNames)
		
saveGameAndCharacter :: Widget RPGState -> WScreenT RPGState
saveGameAndCharacter w=do
	gs@(GameState {gsData=rs@(RPGState {rpgCharacter=(Just c1)})}) <- get
	when (isOutOfService c1) (put (gs{screen=Just (getGameScreen initialScreen)}))
	saveResult<- liftIO $ saveCurrent rs			
	let ws=case saveResult of
		Right _ -> Nothing
		Left t -> Just $ WText t
	return (combineMaybeWidget w ws)
	
listGamesW :: (String -> WScreenT RPGState) -> String -> WScreenT RPGState
listGamesW af name= do
	rs<-gets gsData
	cs<-liftIO $ listGames rs name
	if null cs
		then return (WText "No game created")
	 	else do
	 		let (l,r)=partition (not . isRight) cs 
	 		let l'=(map (\(Left a)->a) l)
	 		let r'=map (\(Right (n,_))->n) r
	 		return (WCombo ("Choose game:":l') r' (af))	

deleteGames :: ActionFunction RPGState 
deleteGames _ =listCharactersW (listGamesW $ deleteFileW deleteGame)

deleteFileW :: (RPGState -> String -> IO (Either String String)) -> String -> WScreenT RPGState 
deleteFileW _ ""=return (WText "Deletion canceled")
deleteFileW f name=return (WCheck ["Delete "++name] "Are you sure?" False (reallyDeleteFileW f name))

reallyDeleteFileW :: (RPGState -> String -> IO (Either String String)) -> Name -> Bool -> WScreenT RPGState 
reallyDeleteFileW _ _ False =  return (WText "Deletion canceled")
reallyDeleteFileW f name True =  do
	rs<-gets gsData
	liftIO $ f rs name
	return (WText (name++" deleted"))
	

playGames :: ActionFunction RPGState
playGames _ =listCharactersW (listGamesW playGame)

playGame :: String -> WScreenT RPGState
playGame ""= return (WText "No game chosen")
playGame shortName= do
	rs <- gets gsData
	r <- withBackup rs shortName
		(\BackupState{bckCharacter=c,bckGame=(Just g)}->do
			createMazeState' c g)
	onError r	
		
				
useItem :: [(Position,ItemType)] -> ActionFunction RPGState
useItem items _=do
	return (if null items
		then (WText "You're not carrying anything to use")
		else (getMappedCombo ppItemPosition' ["What do you want to use?"] items useItemAction)
		)
	
useItemAction ::  ComboResult (Position,ItemType) -> WScreenT RPGState
useItemAction Empty=return (WText "Use canceled")
useItemAction (Unknown _)=return (WText "You can't use that")
useItemAction (Exact item) =do
	rs@(RPGState {rpgCharacter=Just c1}) <- gets gsData
	maybeSC1<- useItemEffect (snd item) c1
	(c2)<- case maybeSC1 of
		Nothing -> do
			addScreenMessage "You can't use that"
			return c1
		Just (c',remove) ->do
			let 
				c2=if remove 
					then
						let Right (inv,_)=Inv.dropItem (inventory $ c1) (fst item)
			 			in c'{inventory=inv}
			 		else c'
			return  (c2)
	let rs2=rs{rpgCharacter=Just c2}
	saveNewGameState rs2
	saveGameAndCharacter (WNothing)
	
castSpell :: [Spell] -> ActionFunction RPGState
castSpell spells _=do
	return (if null spells
		then (WText "You don't know any useful spell")
		else (getMappedCombo spellName ["Which spell do you want to cast?"] spells castSpellAction)
		)

castSpellAction ::  ComboResult Spell -> WScreenT RPGState
castSpellAction Empty=return (WText "Cast canceled")
castSpellAction (Unknown _)=return (WText "You can't cast that")
castSpellAction (Exact spell) =do
	rs@(RPGState {rpgCharacter=Just c1,mgs=Just mgs}) <- gets gsData
	c2<-spellToMyself c1 spell (tickCount mgs)
	tick 5 (rs{rpgCharacter=Just c2})
	saveGameAndCharacter (WNothing)