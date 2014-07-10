-- | Fight resolution
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.Fight 
(giveBlow,FightStatus,swapFS,damageArmor) 

where

import Control.Monad.Writer
import Data.List

import MoresmauJP.Core.Screen
import MoresmauJP.Rpg.Actions
import MoresmauJP.Rpg.Character
import MoresmauJP.Rpg.Items
import MoresmauJP.Rpg.Inventory

import MoresmauJP.Util.Lists
import MoresmauJP.Util.Random

import Text.Printf

type FightStatus a= ((a,a),Bool)


swapFS :: FightStatus a -> FightStatus a
swapFS ((c1,c2),b)=((c2,c1),b)

giveBlow :: (MonadRandom m,MonadWriter [String] m)=>Character -> Character -> m (FightStatus Character)
giveBlow c1 c2 =do 
	((c1b,c2b),rr) <- compete c1 c2  melee
	damages c1b c2b rr

addMessage :: (MonadWriter ScreenMessages m)=> Message -> m ()
addMessage =addScreenMessage . show 
	
damages:: (MonadRandom m,MonadWriter ScreenMessages m)=>Character -> Character -> RollResult -> m (FightStatus Character)
damages a b (Failure {grade=Exceptional}) = 
	do 
		fe<-randomPickp [minBound..maxBound]
		c2<-damagesFumble a fe
		return ((c2,b),isOutOfService c2)
damages a b (Failure {})= do
	addMessage $ Message (a, Miss)
	return ((a,b),False)
damages c1 c2 rr@(Success {grade=gr})= do
	(c11,d1) <- damageWeapon c1 rr
	(c21,d2) <- damageArmor c2 gr
	let total=max 0 (d1-d2+(damageLevel c11 gr))
	addMessage $ Message (c11,Hit total)
	let c22=addCharacteristic' c21 Current Physical (-total)
	return ((c11,c22),isOutOfService c22)

damagesFumble ::(MonadRandom m,MonadWriter ScreenMessages m)=> Character -> FumbleEvent -> m Character
damagesFumble a SelfWound=do 
	(c11,d1) <- damageWeapon a (Success Standard 0)
	(c12,d2) <- damageArmor c11 Standard
	let total=max 0 (d1-d2+(damageLevel c12 Standard))
	let c13=addCharacteristic' c12 Current Physical (-total)
	addMessage $ Message (c13,Fumble SelfWound total "")
	return c13
damagesFumble a DexterityLoss=do 
	let c11=addCharacteristic' a Current Physical (-1)
	let c12=addCharacteristic' c11 Current Dexterity (-1)
	addMessage $ Message (c12,Fumble DexterityLoss 0 "")
	return c12
damagesFumble a CharismaLoss=do 
	let c11=addCharacteristic' a Current Physical (-1)
	let c12=addCharacteristic' c11 Current Charisma (-1)
	addMessage $ Message (c12,Fumble CharismaLoss 0 "")
	return c12
damagesFumble a WeaponBreak=do 
	let 
		i=inventory a
		mis=filter (\(p,_)->p==RightHand || p==LeftHand) $ listCarriedItems i
	if null mis
		then
			damagesFumble a DexterityLoss
		else do 
			mi<-randomPickp mis
			let 
				Right (i2,_)=dropItem i (fst mi)
				wn=itName (snd mi)
				c2=a{inventory=i2}
			addMessage $ Message (c2,Fumble WeaponBreak 0 wn)
			return c2

damageLevel :: Character-> Grade -> Int
damageLevel c Standard = damageBonus c
damageLevel c Remarkable = div ((damageBonus c) * 12) 10
damageLevel c Exceptional = div ((damageBonus c) * 17) 10

damageBonus :: Character -> Int
damageBonus c = div (getCharacteristic' c Current Strength) 4

damageWeapon :: (MonadRandom m,MonadWriter ScreenMessages m)=>Character -> RollResult ->  m (Character,Int)
damageWeapon c rr=do
	-- get items, no duplicate (for two hands weapon)
	let items=nub $ (filter isWeapon) $ listActiveItems (inventory c)
	(c2,items2)<-if length items==2
		then
			do
			-- dexterity roll to see if we could use the second weapon
			(c1,rr)<-action c [Dexterity] (subsequentDifficulty (grade rr))
			if (isSuccess rr)
				then do
					addMessage $ Message(c1,TwoHandedAttack)
					return (c1,items)
				-- if failed, used only right hand weapon	
				else 
					return (c1,[head items])
		else
			return (c,items)
	dmg<-mapM (damageFromItemHigh rr) items2
	return (c2, sum dmg) 

damageArmor :: (MonadRandom m,MonadWriter ScreenMessages m)=>Character -> Grade -> m(Character,Int)
damageArmor c g = do
	let items=nub $ (filter isProtective) $ listActiveItems (inventory c)
	(c2,items2)<-foldM shieldF (c,[]) items
	dmg<-mapM  damageFromItem items2
	return (c2,sum dmg)
	where 
		shieldF :: (MonadRandom m,MonadWriter ScreenMessages m)=> (Character,[ItemType]) -> ItemType -> m (Character,[ItemType])
		shieldF (c,its) it@(Shield {})=do
			-- dexterity roll on the shield
			(c1,rr)<-action c [Dexterity] (-(subsequentDifficulty g))
			if (isSuccess rr)
				then do
					addMessage $ Message(c1,ShieldDefense)
					return (c1,(it:its))
				-- if failed, do not use shields
				else 
					return (c1,its)
		shieldF (c,its) it=return (c,(it:its))
		
damageFromItem ::  (MonadRandom m)=>ItemType -> m(Int)
damageFromItem it =roll (damageLow it,damageHigh it)

damageFromItemHigh::  (MonadRandom m)=>RollResult -> ItemType -> m(Int)
damageFromItemHigh rr it=do
	dmg<-damageFromItem it
	return (resultMultiplierHigh dmg rr) 

newtype Message = Message (Character,MessageType)

data MessageType = Fumble FumbleEvent Int String  
	| Miss | Hit Int | TwoHandedAttack | ShieldDefense
	deriving (Show,Read,Eq)

data FumbleEvent = SelfWound | DexterityLoss | CharismaLoss | WeaponBreak
	deriving (Show,Read,Eq,Bounded,Enum,Ord)

instance Show Message where
	showsPrec _ (Message (c,(Fumble SelfWound damage _))) = showString $ printf "%s fumbles and gives %s %d damages" (name c) (reflective c) damage
	showsPrec _ (Message (c,(Fumble DexterityLoss _ _))) = showString $ printf "%s fumbles and gives %s a hand injury (-1 Dexterity)" (name c) (reflective c)
	showsPrec _ (Message (c,(Fumble CharismaLoss _ _))) = showString $ printf "%s fumbles and gives %s a face injury (-1 Charisma)" (name c) (reflective c)
	showsPrec _ (Message (c,(Fumble WeaponBreak _ s))) = showString $ printf "%s fumbles and breaks %s %s" (name c) (possessive c) s
	showsPrec _ (Message (c,(Miss))) = showString $ printf "%s misses" (name c)
	showsPrec _ (Message (c,(Hit 0))) = showString $ printf "%s hits but causes no damages" (name c)
	showsPrec _ (Message (c,(Hit damage))) = showString $ printf "%s hits and causes %d damages" (name c) damage
	showsPrec _ (Message (c,TwoHandedAttack))= showString $ printf "%s manages a two-hand attack" (name c) 
	showsPrec _ (Message (c,ShieldDefense))= showString $ printf "%s manages to shield" (name c) 

