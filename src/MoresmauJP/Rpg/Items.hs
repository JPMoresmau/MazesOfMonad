-- | Items that can be found in the mazes
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.Items where

import Control.Monad.Writer

import MoresmauJP.Core.Screen
import MoresmauJP.Rpg.Actions
import MoresmauJP.Rpg.Character
import MoresmauJP.Rpg.Inventory
import MoresmauJP.Rpg.Magic
import MoresmauJP.Util.Random


import Text.Printf

sword::ItemType		
sword=Weapon "Sword" 2 6 1 10
twoHandedWord=Weapon "2 Handed Sword" 4 12 2 20
dagger=Weapon "Dagger" 1 4 1 5
hatchet=Weapon "Hatchet" 1 6 1 8
battleaxe=Weapon "Battle Axe" 3 10 2 15
club=Weapon "Club" 1 4 1 2
bigclub=Weapon "Massive Club" 3 10 2 4
smallShield=Shield "Small shield" 1 4 8
bigShield=Shield "Big shield" 3 7 12
leatherArmor=Armor "Leather armor" 2 6 15
chainMail=Armor "Chain mail armor" 4 10 25
fullPlate=Armor "Full plate armor" 5 12 40
leathercap=Helmet "Leather cap" 1 3 5
helmet=Helmet "Helmet" 2 7 10
heaume=Helmet "Full Heaume" 4 10 18

minorHealingPotion=Potion "Minor Healing Potion" (HealingPotion 3) 12
mediumHealingPotion=Potion "Medium Healing Potion" (HealingPotion 6) 20
majorHealingPotion=Potion "Major Healing Potion" (HealingPotion 9) 30
totalHealingPotion=Potion "Total Healing Potion" (HealingPotion 1000) 60

minorMindPotion=Potion "Minor Mind Potion" (MindPotion 3) 12
mediumMindPotion=Potion "Medium Mind Potion" (MindPotion 6) 20
majorMindPotion=Potion "Major Mind Potion" (MindPotion 9) 30
totalMindPotion=Potion "Total Mind Potion" (MindPotion 1000) 60

trapDoor=Trap "Trap Door" 3 10 "A trap door opens under your feet!"
dartLauncher=Trap "Dart Launcher" 2 8 "A dart launches from the wall and hits you!"
boulderFall=Trap "Boulder" 2 12 "A big boulder falls from the ceiling!"


-- occurences of items in the maze
itemOccurences:: [(ItemType,Int)]
itemOccurences=	[(sword,12),
		(twoHandedWord,5),
		(dagger,8),
		(hatchet,12),
		(battleaxe,5),
		(smallShield,10),
		(bigShield,5),
		(leatherArmor,8),
		(chainMail,4),
		(fullPlate,2),
		(leathercap,8),
		(helmet, 5),
		(heaume, 2),
		(minorHealingPotion,8),
		(mediumHealingPotion,5),
		(majorHealingPotion,2),
		(totalHealingPotion,1),
		(minorMindPotion,8),
		(mediumMindPotion,5),
		(majorMindPotion,2),
		(totalMindPotion,1),
		(Scroll "" "" 40,8)
	]

trapOccurences :: [(ItemType,Int)]
trapOccurences = [(trapDoor,4),
		(dartLauncher,3),
		(boulderFall,2)]
	
isWeapon :: ItemType -> Bool
isWeapon (Weapon {})=True
isWeapon _ =False

isProtective :: ItemType -> Bool
isProtective (Armor {})=True
isProtective (Shield {})=True
isProtective (Helmet {})=True
isProtective _ =False

	
isTrap :: ItemType -> Bool
isTrap (Trap{})=True
isTrap _=False	
	
canUseItem :: ItemType -> Bool
canUseItem (Potion {})=True
canUseItem (Scroll {})=True
canUseItem _=False
	
useItemEffect :: (MonadRandom m,MonadWriter ScreenMessages m)=>ItemType -> Character -> m (Maybe (Character,Bool))
useItemEffect (Potion{potionType=(HealingPotion n)}) c1= do
	(c2,rr)<-action c1 medecine (toIntLevel Neutral)
	let n'=resultExtra n rr
	(c3,_)<-recover c2 Physical n'
	return (Just (c3,True))
useItemEffect (Potion{potionType=(MindPotion n)}) c1= do
	(c2,rr)<-action c1 medecine (toIntLevel Neutral)
	let n'=resultExtra n rr
	(c3,_)<-recover c2 Mental n'
	return (Just (c3,True))
useItemEffect (Scroll{spellType=s}) c1@(Character {spells=knownSpells})= do
	if (elem s (map spellName knownSpells))
		then do
			addScreenMessage $ "You already know that spell"
			return (Just (c1,False))
		else do
			(c2,rr)<-action c1 spelllearning (toIntLevel RatherHard)
			case rr of 
				Success{}->do
					addScreenMessage (printf "You learn the spell %s" s)
					return (Just (c2{spells=knownSpells++(filter ((s ==) . spellName) allSpells)},True))
				Failure{grade=Exceptional}->do
					addScreenMessage "You fail badly to learn that spell"
					return (Just (addCharacteristic' c2 Current Mental (-1),True))
				Failure{}->do
					addScreenMessage "You fail to learn that spell"
					return (Just (c2,True))
useItemEffect _ _=return Nothing