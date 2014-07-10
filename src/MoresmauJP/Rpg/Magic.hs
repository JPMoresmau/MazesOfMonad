-- | Magic spells casting resolution
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.Magic where

import Control.Monad.Writer

import MoresmauJP.Core.Screen
import MoresmauJP.Rpg.Actions
import MoresmauJP.Rpg.Character

import MoresmauJP.Util.Lists
import MoresmauJP.Util.Random

import Text.Printf
	
type MagicStatus a= ((a,a),Bool)
	
allSpells :: [Spell]
allSpells= [
	Spell "Feel Better" Physical Recovery Permanent
	,Spell "Fire Ball" Physical Negative Permanent
	,Spell "Nimble Fingers" Dexterity Positive Temporary
	,Spell "Greasy Fingers" Dexterity Negative Temporary 
	,Spell "Madness" Mental Negative Permanent
	,Spell "Sanity" Mental Recovery Permanent
	,Spell "Focus... Focus... Focus..." Willpower Positive Temporary
	,Spell "Think! Think! Think!" Intelligence Positive Temporary
	,Spell "Doh!" Intelligence Negative Temporary
	,Spell "Ox Strength" Strength Positive Temporary
	,Spell "Troll Face" Charisma Negative Temporary
	,Spell "Drop dead gorgeous" Charisma Positive Temporary
	]
	
spellToAffect :: Spell -> RollResult -> Int -> Affect
spellToAffect spell rr tc=let
	pt=resultMultiplierHigh (diff rr) rr
	in Affect (impactedChar spell)
	pt
	(tc + (((diff rr)+1) ^ 2))
	(spellName spell)
	(printf "Under the influence of %s (%s)" (spellName spell) (points (impactedChar spell) pt))
	(printf "Spell %s is lifting" (spellName spell))

spellsToMyself :: Character -> [Spell]
spellsToMyself c=removedAlreadyAffecting c $ filter ((Negative /=) . impact) (spells c)

spellsToOpponent :: Character -> Character -> [Spell]
spellsToOpponent c opponent=removedAlreadyAffecting opponent $ filter ((Negative ==) . impact) (spells c)

removedAlreadyAffecting :: Character -> [Spell] -> [Spell]
removedAlreadyAffecting c spells=let
	affectSources=map source (affects c)
	in filter (\x-> notElem (spellName x) affectSources) spells

spellToMyself ::  (MonadRandom m,MonadWriter ScreenMessages m) => Character -> Spell -> Int ->  m Character
spellToMyself c s tc=do
	(c2,rr)<-action c spellcasting (toIntLevel Neutral)
	spellToMyselfEffect c2 s tc rr

spellToMyselfEffect :: 	(MonadRandom m,MonadWriter ScreenMessages m) => Character -> Spell -> Int -> RollResult -> m Character
spellToMyselfEffect c s tc rr@(Failure {grade=Exceptional})=do
	fe<-randomPickp [ForgetSpell .. maxBound]
	damagesFumble c s tc rr fe
spellToMyselfEffect c s _ (Failure {})=do
	addMessage $ Message (c,s,Fail)
	return c
spellToMyselfEffect c s tc rr@(Success {})=
	case spellDuration s of
		Temporary->do
			addMessage  $ Message (c,s,Myself)
			let aff=(spellToAffect s rr tc)
			addScreenMessage $ affectDescription aff
			return (addAffect c aff)
		Permanent->do
			addMessage $ Message (c,s,Myself)
			(a,_)<-recover c (impactedChar s) (diff rr)
			return a
	
damagesFumble :: (MonadWriter ScreenMessages m) => Character -> Spell -> Int ->  RollResult  -> FumbleEvent -> m Character
damagesFumble c s tc rr SpellBounce= do
	let c12=addAffect c (spellToAffect s (Success Standard (-diff rr)) tc)
	addMessage $ Message (c12,s,Fumble SpellBounce 0)
	return c12
damagesFumble c s _ _ ForgetSpell= do
	let c12=c{spells=filter (s /=) (spells c)}
	addMessage $ Message (c12,s,Fumble ForgetSpell 0)
	return c12
damagesFumble c s _ _ MentalLoss= do
	let c12=addCharacteristic' c Current Mental (-1)
	addMessage $ Message (c12,s,Fumble MentalLoss 1)
	return c12
damagesFumble c s _ _ IntelligenceLoss= do
	let c12=addCharacteristic' c Current Intelligence (-1)
	addMessage $ Message (c12,s,Fumble IntelligenceLoss 1)
	return c12


addMessage :: (MonadWriter ScreenMessages m)=> Message -> m ()
addMessage =addScreenMessage . show 
	
spellToOpponent ::(MonadRandom m,MonadWriter ScreenMessages m)=> Character -> Character -> Spell -> Int -> m (MagicStatus Character)
spellToOpponent c1 c2 s tc= do
	((c1b,c2b),rr) <- compete c1 c2 spellcasting
	spellToOpponentEffect c1b c2b s tc rr
	
spellToOpponentEffect :: (MonadRandom m,MonadWriter ScreenMessages m) => Character -> Character -> Spell -> Int -> RollResult -> m (MagicStatus Character)
spellToOpponentEffect c1 c2 s tc rr@(Failure {grade=Exceptional})=do
	fe<-randomPickp [minBound .. maxBound]
	c1'<-damagesFumble c1 s tc rr fe
	return $ ((c1',c2),isOutOfService c1')
spellToOpponentEffect c1 c2 s _ (Failure {})=do
	addMessage $ Message (c1,s,Fail)
	return $ ((c1,c2),False)
spellToOpponentEffect c1 c2 s tc rr@(Success {})=
	case spellDuration s of
		Temporary->do
			addMessage $ Message (c1,s,Opponent)
			let c2'=addAffect c2 (spellToAffect s rr tc)
			return ((c1,c2'),isOutOfService c2')
		Permanent->do
			addMessage $ Message (c1,s,Opponent)
			let c2'=(addCharacteristic' c2 Current (impactedChar s) (-(diff rr)))
			return ((c1,c2'),isOutOfService c2')
		
-- complexity
-- danger
-- curse: add difficulty to all actions
-- need a time counter to evaluate when spell is finished

newtype Message=Message (Character,Spell,MessageType)

data MessageType=Fumble FumbleEvent Int | Fail |Myself | Opponent

data FumbleEvent = SpellBounce | ForgetSpell | MentalLoss | IntelligenceLoss
	deriving (Show,Read,Eq,Bounded,Enum,Ord)

instance Show Message where
	showsPrec _ (Message (_,s,Fumble MentalLoss a))=showString $ printf "The spell %s backfires on you and causes you to lose %s!" (spellName s) (points Mental a)
	showsPrec _ (Message (_,s,Fumble ForgetSpell _))=showString $ printf "The spell %s backfires and you promptly forget it!" (spellName s)
	showsPrec _ (Message (_,s,Fumble SpellBounce _))=showString $ printf "The spell %s bounces back and hits you!" (spellName s)
	showsPrec _ (Message (_,s,Fumble IntelligenceLoss a))=showString $ printf "The spell %s confuses you and causes you to lose %s!" (spellName s) (points Intelligence a)
	showsPrec _ (Message (_,s,Fail))=showString $printf "The spell %s fails" (spellName s)
	showsPrec _ (Message (_,s,Myself))=showString $printf "The spell %s worked" (spellName s)
	showsPrec _ (Message (c,s,Opponent))=showString $printf "%s casts %s" (name c) (spellName s)
