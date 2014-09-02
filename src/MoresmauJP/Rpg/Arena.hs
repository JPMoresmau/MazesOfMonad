-- | The "arena" is where interaction between the character and a NPC is played out
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.Arena where

import Control.Monad.State
import Control.Monad.Writer

import Data.Maybe
import Data.List

import MoresmauJP.Core.Screen
import MoresmauJP.Rpg.Actions
import MoresmauJP.Rpg.Character
import MoresmauJP.Rpg.Inventory
import MoresmauJP.Rpg.NPC
import MoresmauJP.Rpg.Fight
import MoresmauJP.Rpg.Magic
import MoresmauJP.Rpg.Trade
import MoresmauJP.Rpg.TextOutput
import MoresmauJP.Util.Numbers
import MoresmauJP.Util.Random

import Text.Printf

data ExitStatus=Death | Victory | Flight | Finished | EnnemyFlight
	deriving (Show,Read,Eq,Ord,Enum,Bounded)

data ExitState=ExitState {
	exitCharacter::Character,
	exitOpponent::NPCCharacter,
	exitStatus::ExitStatus,
	newItems::[ItemType],
	exitTickCount::Int
	} deriving (Show,Read)

data Arena=Arena {
	arenaCharacter::Character,
	arenaOpponent::NPCCharacter,
	es::Maybe ExitStatus,
	chToHit::Bool,
	arenaItems::[ItemType],
	arenaTickCount::Int,
	fightInfo::FightInfo
	} deriving (Show,Read)

screenInteract :: Character -> NPCCharacter -> [String]-> RandomWrapper -> Int -> IO (ExitState)
screenInteract c1 c2 ss rw tc= do
	let ar=Arena c1 c2 Nothing True [] tc newFightInfo
	let initialstate=GameState ar (Just (Screen []))
	evalStateT (evalRandT (screenInteract' c2 ss) rw) initialstate

screenInteract' :: NPCCharacter -> [String] -> (RandT (StateT (GameState Arena) IO)) ExitState
screenInteract' c2 ss= do
	let wEncounter=combineWidget (WList ss) (WText (printf "You meet a %s." (ppNPC c2)))
	initial<-getInitialAttitude c2
	(w,msgs)<-case initial of
		NPCFight -> do
			(w,msgs)<-runWriterT fightInArena 
			return (w, (printf "The %s attacks!" (name $ npcCharacter c2)):msgs)
		NPCWait -> runWriterT chooseAction
		NPCTrade ->runWriterT tradeInArena
	let wmsgs=WList ( msgs)
	let w2=combineWidget wmsgs w
	commandLoop2 (combineWidget wEncounter w2)
	(GameState ar2 _)<-get
	return (ExitState (arenaCharacter ar2) (arenaOpponent ar2) (fromJust $ es ar2) (arenaItems ar2) (arenaTickCount ar2))

chooseAction :: WScreenT Arena
chooseAction = do
	gs <- get
	let (Arena{arenaOpponent=npc}) = gsData gs
	return (getShowCombo ["What do you want to do?"] (possibleNPCInteractions npc) chooseF)

chooseF::ComboResult InteractionToNPC -> WScreenT Arena 
chooseF (Exact Fight)=fightInArena
chooseF (Exact Trade)=tradeInArena
chooseF (Exact Convert)=convertInArena
chooseF (Exact Steal)=stealInArena
chooseF _=do
	gs <- get
	let ar = gsData gs
	put (gs{gsData=ar{es=Just Finished},screen=Nothing})
	return WNothing
	
tradeInArena :: WScreenT Arena	
tradeInArena=do
	gs <- get
	let (Arena{arenaCharacter=c1}) = gsData gs
	let gold=printf "You have %d gold coins" (getGold $ inventory c1)
	return (getShowCombo [gold,"Choose trade operation"] [Buy .. Exchange] tradeInArena')
	
tradeInArena' :: ComboResult TradeAction -> WScreenT Arena
tradeInArena' Empty = chooseAction
tradeInArena' (Unknown _)= chooseAction
tradeInArena' (Exact EndTrade)= chooseAction
tradeInArena' (Exact action)= do
	gs <- get
	let ar@(Arena{arenaCharacter=c1,arenaOpponent=npc@NPCCharacter{npcCharacter=c2}}) = gsData gs
	((c1b,c2b),rr)<- competeWithDiff c1 c2 trade (diffAttitude npc)
	let ar2=ar{arenaCharacter=c1b,arenaOpponent=npc{npcCharacter=c2b}}
	put gs{gsData=ar2}
	let ops=listTradeItems c1b c2b action rr
	if null ops
		then do
			w<-tradeInArena
			return $ combineWidget (WText (printf "No good deal you can %s" (show action))) w
		else
			return (getMappedCombo 
				ppTradeOperation'
				[(printf "What do you want to %s" (show action))]
				ops tradeChoice)
	
tradeChoice :: ComboResult TradeOperation -> WScreenT Arena
tradeChoice Empty = tradeInArena
tradeChoice (Unknown _)= tradeInArena
tradeChoice (Exact op)=do
	gs <- get
	let (Arena{arenaCharacter=c1,arenaOpponent=npc}) = gsData gs
	let possiblePos=case op of
		ToBuy (_,it) _-> (listAllowedPositions (inventory c1) it)
		ToExchange (pos,_) (_,it)->  map (\(myPos,it)-> if myPos==pos then (myPos,Nothing) else (myPos,it)) (listAllowedPositions (inventory c1) it)
		ToSell _ _ -> []
	let npcPos=getNpcPosition op (inventory $ npcCharacter npc)
	if (null possiblePos)
		then
			tradeOperation op npcPos Nothing 
		else
			return (getMappedCombo ppInventoryPosition' ["Where do you want to put the item?"] possiblePos (tradeChoice' op npcPos))
	

	
tradeChoice' :: TradeOperation -> Maybe Position -> ComboResult (Position,Maybe ItemType) -> WScreenT Arena
tradeChoice' _ _ Empty = tradeInArena
tradeChoice' _ _ (Unknown _) = tradeInArena
tradeChoice' op posNpc (Exact pos) = tradeOperation op posNpc (Just $ fst pos)
	
tradeOperation :: TradeOperation -> Maybe Position -> Maybe Position -> WScreenT Arena
tradeOperation op posNpc posC = do
	gs <- get
	let ar@(Arena{arenaCharacter=c1,arenaOpponent=npc@NPCCharacter{npcCharacter=c2}}) = gsData gs
	let (c1b,c2b,dropped)=doTradeOperation (c1,posC) (c2,posNpc) op
	let oldDropped=arenaItems ar
	let ar2=ar{arenaCharacter=c1b,arenaOpponent=npc{npcCharacter=c2b},arenaItems=dropped++oldDropped}
	arenaTick ar2
	tradeInArena

arenaTick :: Arena -> WriterT ScreenMessages (RandT (StateT (GameState Arena) IO)) ()
arenaTick ar=do
	gs <- get
	put gs{gsData=ar{arenaTickCount=((arenaTickCount ar)+1)}}
	
fightInArena :: WScreenT Arena
fightInArena = 	do
	gs <- get
	let ar@(Arena {arenaCharacter=c1,arenaOpponent=c2}) = gsData gs
	((c1b,c2b),rr) <- compete c1 (npcCharacter c2) initiative
	let att=bindInt (1,8) ((npcAttitude c2)-2)
	let ar2=ar{arenaCharacter=c1b,arenaOpponent=(c2{npcCharacter=c2b,npcAttitude=att})}
	put (gs{gsData=ar2})
	case rr of
		Success {} -> choiceArena
		_ -> do
			put (gs{gsData=ar2{chToHit=False}})
			fightInArena'

fightInArena' :: WScreenT Arena
fightInArena' = do
	gs <- get
	let ar@(Arena {arenaCharacter=c1,arenaOpponent=c2,chToHit=chToHit,fightInfo=fightInfo}) = gsData gs
	-- here NPC can decide to escape, bribe or pray if chToHit==false
	(fightAttitude,fi2)<-if not chToHit
		then getFightAttitude c2 fightInfo
		else return (ContinueFight,fightInfo)
	let ar2=ar{fightInfo=fi2}
	put (gs{gsData=ar2})
	case fightAttitude of
		ContinueFight -> do
			((c1b,c2b),isDead) <- (case chToHit of
				True -> giveBlow c1 (npcCharacter c2)
				False -> liftM swapFS (giveBlow (npcCharacter c2) c1))
			let npc2=(c2{npcCharacter=c2b})
			let fi3=addNPCLosses (addCharacterLosses fi2 c1 c1b) c2 npc2
			let ar3=ar2{arenaCharacter=c1b,arenaOpponent=npc2,chToHit=not chToHit,fightInfo=fi3}
			arenaTick ar3
			--let w=WList (map show msgs)
			if isDead
				then
					do
					let (status,txt)=if (isOutOfService c1b)
							then (Death,"You're killed!")
							else (Victory,"You triumph!")
					put (gs{gsData=ar3{es=Just status},screen=Nothing})
					return (WText txt)
				else
					do
					w2<- if (not chToHit)
						then choiceArena
						else fightInArena'
					return w2
		OfferBribe gold -> do
			let txt=printf "The %s offers you %d gold coins for your mercy!" (name $ npcCharacter c2) gold
			return $ WCheck [txt] "Do you accept?" True (bribeResponse gold)
		PrayForClemency -> do
			let txt=printf "The %s begs you for mercy!" (name $ npcCharacter c2)
			return $ WCheck [txt] "Do you accept?" True prayResponse
		TryEscape -> do
			-- attitude gets better: if the npc escaped, he'll be less likely to attack next time
			let npc2=c2{npcAttitude=max 20 ((npcAttitude c2)+2)}
			put (gs{gsData=ar2{arenaOpponent=npc2,es=Just EnnemyFlight},screen=Nothing})
			let txt=printf "The %s escapes!" (name $ npcCharacter c2)
			return (WText txt)

bribeResponse :: Int -> Bool -> WScreenT Arena
bribeResponse _ False= choiceArena
bribeResponse gold True= do
	gs <- get
	let ar@(Arena {arenaCharacter=c1,arenaOpponent=npc2}) = gsData gs
	let c2=addCharacterGold (npcCharacter npc2) (-gold)
	let me2=addCharacterGold c1 gold
	let ar2=ar{arenaCharacter=me2,arenaOpponent=npc2{npcAttitude=20,npcCharacter=c2}}
	put (gs{gsData=ar2{es=Just Finished},screen=Nothing})
	let txt=printf "You take the gold and let the %s go!" (name $ c2)
	return (WText txt)

prayResponse :: Bool -> WScreenT Arena
prayResponse False= choiceArena
prayResponse True= do
	gs <- get
	let ar@(Arena {arenaOpponent=c2}) = gsData gs
	let ar2=ar{arenaOpponent=c2{npcAttitude=20}}
	put (gs{gsData=ar2{es=Just Finished},screen=Nothing})
	let txt=printf "You let the %s go!" (name $ npcCharacter c2)
	return (WText txt)
	

castInArena :: WScreenT Arena
castInArena = do
	(Arena {arenaCharacter=c1,arenaOpponent=c2}) <- gets gsData
	let spells=spellsToOpponent c1 (npcCharacter c2)
	if (null spells)
		then do
			w<-choiceArena
			return (combineWidget (WText "No spell you can cast!") w)
		else return (getMappedCombo spellName ["Cast spell:"] spells castInArena' )

castInArena' :: (ComboResult Spell) -> WScreenT Arena
castInArena' Empty = choiceArena
castInArena' (Unknown _)= choiceArena
castInArena' (Exact spell)=do
	gs <- get
	let ar@(Arena {arenaCharacter=c1,arenaOpponent=c2,arenaTickCount=tc,fightInfo=fightInfo}) = gsData gs
	((c1b,c2b),dead) <- spellToOpponent c1 (npcCharacter c2) spell tc
	let npc2=(c2{npcCharacter=c2b})
	let fi2=addNPCLosses (addCharacterLosses fightInfo c1 c1b) c2 npc2
	let ar2=ar{arenaCharacter=c1b,arenaOpponent=npc2,chToHit=False,fightInfo=fi2}
	arenaTick ar2
	--let w=WList (map show msgs)
	if dead
		then
			do
			let (status,txt)=if (isDead c1b)
					then (Death,"You're killed!")
					else if (isMad c1b) 
						then (Death,"You become mad!")
						else (Victory,"You triumph!")
			put (gs{gsData=ar2{es=Just status},screen=Nothing})
			return $ WText txt
		else
			do
			w2<- fightInArena'
			return w2

prayerInArena :: WScreenT Arena
prayerInArena = do
	gs <- get
	let ar@(Arena {arenaCharacter=c1,arenaOpponent=c2,arenaTickCount=tc}) = gsData gs
	((c1b,c2b),rr)<-competeWithDiff c1 (npcCharacter c2) pray (diffAttitude c2)
	let npc2=c2{npcCharacter=c2b}
	let ar2=ar{arenaCharacter=c1b,arenaOpponent=npc2,arenaTickCount=tc+1}
	put (gs{gsData=ar2})
	case rr of
		Failure {grade=Exceptional}->  do
			let npc3=npc2{npcAttitude=1}
			put (gs{gsData=ar2{arenaOpponent=npc3,chToHit=False}})
			w<-fightInArena' 
			return (combineWidget (WText (printf "The %s doesn't like your arguments at all!" (name $ npcCharacter c2))) w)
		Failure {}->do
			put (gs{gsData=ar2{chToHit=False}})
			w<-fightInArena' 
			return (combineWidget (WText (printf "Your prayer failed to convince the %s!" (name c2b))) w)
		rr->do
			let npc3=npc2{npcAttitude=min 20 ((npcAttitude npc2)+diff rr)}
			put (gs{gsData=ar2{es=Just Finished,arenaOpponent=npc3},screen=Nothing})
			return (WText (printf "The %s takes pity on you and leaves you alone" (name c2b)))

diffAttitude :: NPCCharacter -> Int
diffAttitude c2=(div ((npcAttitude c2)-10) 2)

bribeInArena :: WScreenT Arena
bribeInArena = do
	gs <- get
	let ar@(Arena {arenaCharacter=c1,arenaOpponent=c2,arenaTickCount=tc}) = gsData gs
	let currentGold=(getGold $ inventory c1)
	if currentGold==0
		then
			do
			w<-choiceArena
			return (combineWidget (WText "You have no gold!") w)
		else
			do
			((c1b,c2b),rr)<-competeWithDiff c1 (npcCharacter c2) trade (diffAttitude c2)
			let npc2=c2{npcCharacter=c2b}
			let ar2=ar{arenaCharacter=c1b,arenaOpponent=npc2,arenaTickCount=tc+1}
			put (gs{gsData=ar2})
			case rr of
				Failure {grade=Exceptional} ->  do
					let npc3=npc2{npcAttitude=1}
					put (gs{gsData=ar2{arenaOpponent=npc3,chToHit=False}})
					w<-fightInArena' 
					return (combineWidget (WText (printf "The %s doesn't like your money at all!" (name $ npcCharacter c2))) w)
				Failure {}->do
					put (gs{gsData=ar2{chToHit=False}})
					w<-fightInArena' 
					return (combineWidget (WText (printf "The %s doesn't want your money!" (name c2b))) w)
				rr	-> do
					let 
						gold=bindInt (1,currentGold) $ resultMultiplierLow currentGold rr
						c1c=c1b{inventory=addGold (inventory c1b) (-gold)}
						npc3=c2{npcCharacter=c2b{inventory=addGold (inventory c2b) gold}}
					put (gs{gsData=ar2{es=Just Finished,arenaCharacter=c1c,arenaOpponent=npc3},screen=Nothing})
					return (WText (printf "The %s takes your money (%d gold coins) and leaves you alone" (name c2b) gold))

choiceArena :: WScreenT Arena
choiceArena = do
	ar<-gets gsData
	let status=getHealthSummary (arenaCharacter ar)
	let statusOpp=printf "You're fighting %s." (ppNPC (arenaOpponent ar))
	let actions=getPossibleArenaActions (arenaCharacter ar) (arenaOpponent ar)
	return (getShowCombo (["Your health:"]++status++[statusOpp]++["Choose next action"]) actions choiceArenaF)

choiceArenaF :: ComboResult ArenaAction -> WScreenT Arena
choiceArenaF (Exact Escape)=do
	gs<-get
	let ar=gsData gs
	put (GameState (ar{es=Just Flight}) Nothing)
	return (WText "You escape!")
choiceArenaF (Exact Melee)=fightInArena'
choiceArenaF (Exact Magic)=castInArena
choiceArenaF (Exact Prayer)=prayerInArena
choiceArenaF (Exact Bribe)=bribeInArena
choiceArenaF _=choiceArena

data ArenaAction = Melee | Escape | Magic | Prayer | Bribe
	deriving (Show,Read,Enum,Bounded,Eq,Ord)
	
getPossibleArenaActions::Character -> NPCCharacter -> [ArenaAction]
getPossibleArenaActions c1 np1=let
	spells=spellsToOpponent c1 (npcCharacter np1)
	gold=getGold $ inventory c1
	in 
		[Melee,Escape] ++ 
		(if null spells then [] else [Magic]) ++
		(case npcType np1 of 
			Animal -> []
			_ -> [Prayer] ++ (if gold>0 then [Bribe] else []))
			
{--	
getPossibleArenaActions::Gold -> NPCCharacter -> [ArenaAction]
getPossibleArenaActions g (NPCCharacter{npcType=tp})=case tp of 
	Animal 		-> [Melee .. Magic]
	Humanoid 	-> if g>0 then [Melee .. Bribe] else  [Melee .. Prayer]
	Human 	-> if g>0 then [Melee .. Bribe] else  [Melee .. Prayer]
	--}
	
convertInArena :: WScreenT Arena	
convertInArena=do
	gs <- get
	let ar@(Arena {arenaCharacter=c1,arenaOpponent=c2,arenaTickCount=tc}) = gsData gs
	((c1b,c2b),rr)<-compete c1 (npcCharacter c2) conversion
	let npc2=c2{npcCharacter=c2b}
	let ar2=ar{arenaCharacter=c1b,arenaOpponent=npc2,arenaTickCount=tc+1}
	put (gs{gsData=ar2})
	case rr of
		Failure {grade=Exceptional}->  do
			let npc3=npc2{npcAttitude=1}
			put (gs{gsData=ar2{arenaOpponent=npc3,chToHit=False}})
			w<-fightInArena' 
			return (combineWidget (WText (printf "The %s doesn't like your arguments at all and attacks!" (name $ npcCharacter c2))) w)
		Failure {}->do
			put (gs{gsData=ar2{es=Just Finished},screen=Nothing})
			return (WText (printf "Your arguments failed to convince the %s!" (name c2b)))
		rr->do
			gained<-getItemsFromNPC rr listCarriedItemsUniqueObject
			let msg=(printf "The %s finds your religion attractive " (name c2b)) ++
				(case gained of
					(0,0)->"but has nothing to give you"
					(0,gold)->(printf "and gives you %d gold coins" gold)
					(ic,0)->(printf "and abandons %d items, pick what you want" ic)
					(ic,gold)->(printf ", gives you %d gold coins and abandons %d items, pick what you want" gold ic)
				)
			gs2 <- get
			let ar2= gsData gs2
			let npc3=arenaOpponent ar2
			let npc4=npc3{npcAttitude=min 20 ((npcAttitude npc3)+diff rr)}
			put (gs2{gsData=ar2{es=Just Finished,arenaOpponent=npc4},screen=Nothing})
			return (WText msg)

stealInArena :: WScreenT Arena	
stealInArena=do
	gs <- get
	let ar@(Arena {arenaCharacter=c1,arenaOpponent=c2,arenaTickCount=tc}) = gsData gs
	((c1b,c2b),rr)<-compete c1 (npcCharacter c2) steal
	let npc2=c2{npcCharacter=c2b}
	put (gs{gsData=ar{arenaCharacter=c1b,arenaOpponent=npc2,arenaTickCount=tc+1}})
	case rr of
		Failure {grade=Exceptional}->  do
			let npc3=npc2{npcAttitude=1}
			put (gs{gsData=ar{arenaOpponent=npc3,chToHit=False}})
			w<-fightInArena'
			return (combineWidget (WText (printf "The %s feels your fingers and attacks!" (name $ npcCharacter c2))) w)
		Failure {}->do
			let npc3=npc2{npcAttitude=max 1 ((npcAttitude npc2)-diff rr)}
			put (gs{gsData=ar{es=Just Finished,arenaOpponent=npc3},screen=Nothing})
			return (WText (printf "You fail to steal anything from the the %s!" (name c2b)))
		rr->do
			gained<-getItemsFromNPC rr ((filter (\(p,_)->not $ isActive p)) . listCarriedItemsUniqueObject)
			let msg=
				(case gained of
					(0,0)->"You find nothing valuable to steal"
					(0,gold)->(printf "You steal %d gold coins" gold)
					(ic,0)->(printf "You steal %d items, pick what you want" ic)
					(ic,gold)->(printf "You steal %d gold coins and %d items, pick what you want" gold ic)
				)
			gs2 <- get
			let ar2 = gsData gs2
			put (gs2{gsData=ar2{es=Just Finished},screen=Nothing})
			return (WText msg)

getItemsFromNPC:: RollResult -> (Inventory -> [(Position,ItemType)]) ->WriterT ScreenMessages (RandT (StateT (GameState Arena) IO)) (Int,Gold)	
getItemsFromNPC rr f= do
	gs <- get
	let 
		ar@(Arena {arenaOpponent=c2,arenaItems=its1}) = gsData gs
		c2b=npcCharacter c2
		carried=f $ inventory $ c2b 
		theoryItemCount=case (grade rr) of
			Exceptional->4
			Remarkable->2
			Standard->1
		itemCount=min theoryItemCount (length $ carried)
		gold=min ((theoryItemCount - itemCount)*(case (grade rr) of
			Exceptional->12+(diff rr)*4
			Remarkable->6+(diff rr)*2
			Standard->3+(diff rr))) (getGold $ inventory $ npcCharacter c2)
		(i2,its)=foldr (\item (inv,its)-> let Right(inv2,Just it)=dropItem inv (fst item)
			in (inv2,it:its)) ((inventory $ npcCharacter c2),[]) (take itemCount carried)
		i3=addGold i2 (-gold)
		i4=addGold (inventory $ arenaCharacter ar) gold
	put (gs{gsData=ar{arenaCharacter=(arenaCharacter ar){inventory=i4}, arenaOpponent=c2{npcCharacter=c2b{inventory=i3}},arenaItems=its1++its}})
	return (itemCount,gold)
