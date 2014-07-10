-- | NPC template definitions and character creation
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.NPC where

import Control.Monad

import Data.Array.IArray
import Data.Maybe
import Data.List
import qualified Data.Map as M
import MoresmauJP.Rpg.Actions
import MoresmauJP.Rpg.Inventory
import MoresmauJP.Rpg.Items
import MoresmauJP.Rpg.Character
import MoresmauJP.Util.Lists
import MoresmauJP.Util.Numbers
import MoresmauJP.Util.Random

troll=NPCTemplate {
	typeName="Troll",
	creatureType=Humanoid,
	attitudeRange=(3,8),
	traitRanges=[
		(Strength,(18,24)),
		(Dexterity,(7,11)),
		(Constitution,(16,26)),
		(Willpower,(10,12)),
		(Intelligence,(4,8)),
		(Balance,(4,8)),
		(Charisma,(3,5)),
		(Perception,(4,8))],
	possibleItems=[
		(RightHand,[
			(Nothing,3),
			(Just club,2),
			(Just bigclub,3)]
			)],
	possibleGold=(0,10)		
	}

goblin=NPCTemplate {
	typeName="Goblin",
	creatureType=Humanoid,
	attitudeRange=(5,10),
	traitRanges=[
		(Strength,(5,11)),
		(Dexterity,(8,13)),
		(Constitution,(7,12)),
		(Willpower,(5,8)),
		(Intelligence,(6,10)),
		(Balance,(4,8)),
		(Charisma,(3,7)),
		(Perception,(4,8))],
	possibleItems=[
		(RightHand,[
			(Just sword,2),
			(Just dagger,2),
			(Just hatchet,1)]
			),
		(LeftHand, [
			(Nothing,2),
			(Just smallShield,1)]
			),
		(Body, [
			(Nothing,2),
			(Just leatherArmor,1)
			]
			)
		],
	possibleGold=(0,30)	
	}

kobold=NPCTemplate {
	typeName="Kobold",
	creatureType=Humanoid,
	attitudeRange=(5,12),
	traitRanges=[
		(Strength,(4,8)),
		(Dexterity,(8,11)),
		(Constitution,(4,10)),
		(Willpower,(5,8)),
		(Intelligence,(7,12)),
		(Balance,(6,12)),
		(Charisma,(5,9)),
		(Perception,(8,12))],
	possibleItems=[
		(RightHand,[
			(Just sword,1),
			(Just dagger,3),
			(Just hatchet,1)]
			)], 
	possibleGold=(0,7)		
	}



giantRat=NPCTemplate {
	typeName="Giant Rat",
	creatureType=Animal,
	attitudeRange=(5,10),
	traitRanges=[
		(Strength,(3,6)),
		(Dexterity,(4,8)),
		(Constitution,(3,7)),
		(Willpower,(2,4)),
		(Intelligence,(2,3)),
		(Balance,(5,10)),
		(Charisma,(1,2)),
		(Perception,(4,8))],
	possibleItems=[],
	possibleGold=(0,0)	
	}

snake=NPCTemplate {
	typeName="Snake",
	creatureType=Animal,
	attitudeRange=(5,10),
	traitRanges=[
		(Strength,(5,8)),
		(Dexterity,(12,16)),
		(Constitution,(5,10)),
		(Willpower,(6,10)),
		(Intelligence,(2,4)),
		(Balance,(5,10)),
		(Charisma,(1,4)),
		(Perception,(8,10))],
	possibleItems=[],
	possibleGold=(0,0)	
	}	
	
scorpion=NPCTemplate {
	typeName="Giant Scorpion",
	creatureType=Animal,
	attitudeRange=(4,8),
	traitRanges=[
		(Strength,(12,18)),
		(Dexterity,(14,20)),
		(Constitution,(15,25)),
		(Willpower,(12,16)),
		(Intelligence,(4,6)),
		(Balance,(10,12)),
		(Charisma,(1,4)),
		(Perception,(10,14))],
	possibleItems=[],
	possibleGold=(0,0)	
	}		
	
vampire=NPCTemplate {
	typeName="Vampire",
	creatureType=Humanoid,
	attitudeRange=(3,8),	
	traitRanges=[
		(Strength,(12,18)),
		(Dexterity,(10,14)),
		(Constitution,(16,24)),
		(Willpower,(14,22)),
		(Intelligence,(14,20)),
		(Balance,(8,14)),
		(Charisma,(10,15)),
		(Perception,(6,10))],
	possibleItems=[],
	possibleGold=(10,60)	
	}
	
ghoul=NPCTemplate {
	typeName="Ghoul",
	creatureType=Humanoid,
	attitudeRange=(1,6),	
	traitRanges=[
		(Strength,(8,14)),
		(Dexterity,(8,12)),
		(Constitution,(18,24)),
		(Willpower,(10,14)),
		(Intelligence,(6,10)),
		(Balance,(6,10)),
		(Charisma,(4,8)),
		(Perception,(3,7))],
	possibleItems=[],
	possibleGold=(0,20)	
	}

outlaw=NPCTemplate {
	typeName="Outlaw",
	creatureType=Human,
	attitudeRange=(8,12),	
	traitRanges=[
		(Strength,(8,14)),
		(Dexterity,(8,14)),
		(Constitution,(8,12)),
		(Willpower,(5,8)),
		(Intelligence,(6,10)),
		(Balance,(8,12)),
		(Charisma,(6,10)),
		(Perception,(10,15))],
	possibleItems=[(RightHand,[
			(Just sword,2),
			(Just hatchet,1)]
			),
		(LeftHand, [
			(Nothing,1),
			(Just smallShield,1),
			(Just dagger,2)
			]
			),
		(Body, [
			(Nothing,2),
			(Just leatherArmor,2),
			(Just chainMail,1)
			]
			),
		(Head,[
			(Nothing, 3),
			(Just leathercap,1),
			(Just helmet,1)
			])],
		possibleGold=(0,50)		
	}

blackKnight=NPCTemplate {
	typeName="Black Knight",
	creatureType=Human,
	attitudeRange=(5,10),	
	traitRanges=[
		(Strength,(14,18)),
		(Dexterity,(14,18)),
		(Constitution,(14,18)),
		(Willpower,(10,15)),
		(Intelligence,(8,12)),
		(Balance,(8,12)),
		(Charisma,(8,14)),
		(Perception,(8,14))],
	possibleItems=[(RightHand,[
			(Just sword,2),
			(Just twoHandedWord,1),
			(Just battleaxe,1)]
			),
		(LeftHand, [
			(Nothing,1),
			(Just smallShield,2),
			(Just bigShield,1),
			(Just dagger,2)
			]
			),
		(Body, [
			(Just leatherArmor,3),
			(Just chainMail,2),
			(Just fullPlate,1)
			]
			),
		(Head,[
			(Nothing, 1),
			(Just helmet,2),
			(Just heaume,1)
			])],
	possibleGold=(10,30)	
	}
	
darkElf=NPCTemplate {
	typeName="Dark Elf",
	creatureType=Human,
	attitudeRange=(5,10),	
	traitRanges=[
		(Strength,(12,18)),
		(Dexterity,(16,22)),
		(Constitution,(10,16)),
		(Willpower,(10,15)),
		(Intelligence,(15,22)),
		(Balance,(10,14)),
		(Charisma,(12,20)),
		(Perception,(16,22))],
	possibleItems=[(RightHand,[
			(Just sword,3),
			(Just dagger,1),
			(Just hatchet,1)]
			),
		(LeftHand, [
			(Nothing,2),
			(Just sword,2),
			(Just dagger,4),
			(Just hatchet,4)]
			),
		(Body, [
			(Just leatherArmor,1),
			(Nothing,2)	]
			)],
	possibleGold=(10,30)	
	}	

minotaur=NPCTemplate {
	typeName="Minotaur",
	creatureType=Animal,
	attitudeRange=(2,5),	
	traitRanges=[
		(Strength,(14,22)),
		(Dexterity,(10,16)),
		(Constitution,(14,22)),
		(Willpower,(10,16)),
		(Intelligence,(6,10)),
		(Balance,(10,14)),
		(Charisma,(4,8)),
		(Perception,(5,10))],
	possibleItems=[],
	possibleGold=(0,0)
	}

peddler=NPCTemplate {
	typeName="Peddler",
	creatureType=Human,
	attitudeRange=(20,20),
	traitRanges=[
		(Strength,(6,12)),
		(Dexterity,(8,13)),
		(Constitution,(8,14)),
		(Willpower,(5,12)),
		(Intelligence,(8,14)),
		(Balance,(6,12)),
		(Charisma,(10,14)),
		(Perception,(8,12))],
	possibleItems=[
		(RightHand,[
			(Nothing,3),
			(Just dagger,2),
			(Just sword,3)]
			),
		(Body, [
			(Nothing,3),
			(Just leatherArmor,1)
			]
			)],
	possibleGold=(100,200)		
	}


allNPCTemplates :: [NPCTemplate]
allNPCTemplates= [troll,goblin,kobold,giantRat,snake,scorpion,
	vampire,ghoul,outlaw,blackKnight,minotaur,peddler,darkElf]

templateAttitudeNoChange=[peddler]

getNPCOccurences :: Int -> [(NPCTemplate,Int)]
getNPCOccurences characterLevel=
	let 
		totalTemplates=length allNPCTemplates
		ponderedLevel=div (characterLevel * 8) 10
		likelyhood npc=(npc,max 1 (totalTemplates - (2 * (abs ((templateLevel npc) - ponderedLevel)))))
	in map likelyhood allNPCTemplates
		
templateLevel :: NPCTemplate -> Int
templateLevel t=avg $ map (\(_,(low,high))-> div (low+high) 2) (traitRanges t)
		
type CharacteristicRanges=Array Characteristic (Int,Int)

data NPCTemplate = NPCTemplate {
	typeName::Name,
	creatureType::NPCType,
	attitudeRange::(Int,Int),
	traitRanges::[(Characteristic,(Int,Int))],
	possibleItems::[(Position,[(Maybe ItemType,Int)])],
	possibleGold::(Gold,Gold)
	} deriving (Show,Read,Eq)

data NPCCharacter = NPCCharacter {
	npcCharacter::Character,
	npcType::NPCType,
	npcAttitude::Int
	}deriving (Show,Read,Eq)

data NPCType=Animal | Humanoid | Human
	deriving (Show,Read,Eq,Ord,Enum,Bounded)

{--data AttitudeLevel=Rather | Quite | Very
	deriving (Show,Read,Eq,Ord,Enum,Bounded)

data Attitude=Friendly | Neutral | Aggressive | Hostile
	deriving (Show,Read,Eq,Ord,Enum,Bounded)

allAttitudeLevels=[(x,y) | y<-[Friendly .. Hostile], x<-[Rather .. Very]]
--}

data InteractionFromNPC=NPCFight | NPCTrade | NPCWait
	deriving (Show,Read,Eq,Enum,Bounded)

getInitialAttitude :: (MonadRandom m)=> NPCCharacter -> m (InteractionFromNPC)
getInitialAttitude NPCCharacter{npcAttitude=att}=do
	r <- (roll d20)
	let (rr,_)=evalResult r att
	return (case rr of
		Failure {}	-> NPCFight
		Success	{}	-> NPCWait)

data FightAttitude = ContinueFight | OfferBribe Int | PrayForClemency | TryEscape 
	deriving (Show,Read,Eq)

data FightInfo=FightInfo {
		npcLosses :: M.Map Characteristic Int,
		cLosses :: M.Map Characteristic Int,
		hasPrayed :: Bool,
		hasBribed :: Bool
	}
	deriving (Show,Read,Eq)

newFightInfo :: FightInfo
newFightInfo=FightInfo M.empty M.empty False False

addNPCLosses:: FightInfo -> NPCCharacter -> NPCCharacter -> FightInfo
addNPCLosses fi c1 c2=fi{npcLosses=addLosses (npcLosses fi) (npcCharacter c1) (npcCharacter c2)}

addNPCLoss:: FightInfo -> Characteristic -> Int -> FightInfo
addNPCLoss fi c v=fi{npcLosses=addToFightInfo (npcLosses fi) (c,v)}

addCharacterLoss:: FightInfo -> Characteristic -> Int -> FightInfo
addCharacterLoss fi c v=fi{cLosses=addToFightInfo (cLosses fi) (c,v)}

addCharacterLosses:: FightInfo -> Character -> Character -> FightInfo
addCharacterLosses fi c1 c2=fi{cLosses=addLosses (cLosses fi) c1 c2}


addLosses :: M.Map Characteristic Int -> Character -> Character -> M.Map Characteristic Int
addLosses m1 c1 c2= let
	both=zipWith (\(c1,r1) (_,r2) -> (c1,(getR Current r1)-(getR Current r2))) (assocs $ traits c1) (assocs $ traits c2)
	changed=filter (\((_,diff))->diff>0) both
	in foldl addToFightInfo m1 changed

addToFightInfo :: M.Map Characteristic Int -> (Characteristic, Int) -> M.Map Characteristic Int
addToFightInfo m1 (c,v)=let
	i=M.lookup c m1
	in case i of
		Nothing -> M.insert c v m1
		Just v1 -> M.insert c (v+v1) m1

getFightAttitudes :: NPCCharacter -> FightInfo -> [(FightAttitude,Int)]
getFightAttitudes npc fi=let 
	fight=21-(npcAttitude npc)
	percentPhysical=getCurrentPercentOfNormal (npcCharacter npc) Physical
	percentMental=getCurrentPercentOfNormal (npcCharacter npc) Mental
	npcLossesSum=(M.fold (+) 0 (npcLosses fi))
	cLossesSum=(M.fold (+) 0 (cLosses fi))
	percentLoss=if npcLossesSum>0
		then (cLossesSum * 100) `div` npcLossesSum
		else 100
	escape=max 0 (if (percentPhysical<11)
		then 10+(11-percentPhysical)
		else if (percentMental<11)
			then 10+(11-percentMental)
			else 10-((percentPhysical + percentMental) `div` 10))
		+ (if (percentLoss<80)
			then (10 - (percentLoss `div` 10))
			else 0
		)
	pray=if (hasPrayed fi || (npcType npc)==Animal) 
		then 0
		else div escape 2
	gold=getGold $ inventory $ npcCharacter npc
	bribe=if gold>0 && (not $ hasBribed fi)  && ((npcType npc)/=Animal)
		then div escape 2
		else 0
	in [(ContinueFight,fight),(TryEscape,escape),(PrayForClemency,pray),(OfferBribe gold,bribe)]

getFightAttitude :: (MonadRandom m)=> NPCCharacter -> FightInfo -> m (FightAttitude,FightInfo)
getFightAttitude npc fi = do
	fa<-randomPickp $ occurenceList $ getFightAttitudes npc fi
	let fi2= case fa of
		PrayForClemency -> fi{hasPrayed=True}
		OfferBribe _->fi{hasBribed=True}
		_ -> fi
	return (fa,fi2)

data InteractionToNPC=Ignore | Fight | Trade | Convert | Steal 
	deriving (Show,Read,Eq,Enum,Bounded)

possibleNPCInteractions :: NPCCharacter -> [InteractionToNPC]
possibleNPCInteractions NPCCharacter{npcType=tp}=case tp of 
	Animal 		-> [Ignore .. Fight]
	Humanoid 	-> [Ignore .. Trade]
	Human 		-> [Ignore .. Steal]

maxPosBag :: NPCTemplate -> Int
maxPosBag nt=case creatureType nt of
	Animal 		-> 0
	Humanoid 	-> 0
	Human 		-> 10

generateFromTemplate :: (MonadRandom m)=> NPCTemplate -> Int -> m (NPCCharacter)
generateFromTemplate template attitudeModifier=do
	let name=typeName template
	-- get gender
	rnd <- getRandomRange (1,2)
	let gender= if rnd==1
		then Male
		else Female
	-- generate ratings
	ranges<-mapM generateFromRange (sortBy (\x y-> compare (fst x) (fst y)) (traitRanges template))
	-- fill ratings with health
	let traits=getDefaultHealth $ array (head allCharacteristics,last allCharacteristics) ranges
	-- get item or Nothing for each position
	inv<-mapM (\(pos,items)->do
		it<-randomPickp (occurenceList items)
		return (pos,it)) (possibleItems template)
	-- only keep items, filter out Nothings
	let inv2=map (\(x,y)->(x,fromJust y)) (filter (isJust . snd) inv)
	gold<-getRandomRange $ possibleGold template
	attitudeLevel<-liftM (	if (elem template templateAttitudeNoChange)
			then id
			else ((max 1) . (\x->x - attitudeModifier)))
		(getRandomRange $ attitudeRange template)
	let maxPos=maxPosBag template
	return (NPCCharacter (Character {
		name=name,
		gender=gender,
		traits=traits,
		inventory=makeFullInventory inv2 maxPos gold,
		affects=[],
		spells=[]
	}) (creatureType template) attitudeLevel)
	
generateFromRange :: (MonadRandom m)=>(Characteristic,(Int,Int))->m (Characteristic,Rating)
generateFromRange (c,(low,high))=do 
	rnd<-getRandomRange (low,high)
	return (c,mkRating rnd)