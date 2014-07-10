-- | Character handling
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.Character where

import Control.Monad.Writer


import Data.Array.IArray
import Data.List
import Data.Ord

import Text.Printf
import Text.Regex.Posix

import MoresmauJP.Core.Screen
import MoresmauJP.Rpg.Inventory
import MoresmauJP.Util.Numbers


type Name=String
type CharacteristicRatings=Array Characteristic Rating

data Character = Character {
	name::Name
	,gender::Gender
	,traits::CharacteristicRatings
	,inventory::Inventory
	, affects::[Affect]
	, spells::[Spell]
	} deriving (Eq,Show,Read)
	
data Gender = Male 
	| Female
	deriving (Show, Read,Eq)

data Affect=Affect {
	affected::Characteristic,
	affect::Int,
	untilTick::Int,
	source::String,
	affectDescription::String,
	liftDescription::String
	}
	deriving (Show, Read,Eq)

data Characteristic = Strength | Dexterity | Constitution | Willpower
	| Intelligence | Balance | Charisma | Perception | Physical | Mental
	deriving (Show,Read,Eq,Enum,Bounded,Ord,Ix)
	
data RatingScoreType=Normal |
	Current |
	Experience
	deriving (Show,Read,Enum,Eq,Ord,Bounded,Ix)

data RatingScore=RatingScore RatingScoreType Int
	deriving (Show,Read,Eq)

data Rating=Rating (Array RatingScoreType Int)
	deriving (Eq)

instance Show Rating where
	show r=printf "%d/%d(%d)" (getR Current r) (getR Normal r) (getR Experience r)
	
instance Read Rating where
	readsPrec _ s= let
		(_,_,after,(c:n:e:[]))= (s=~"([0-9]+)/([0-9]+)\\(([0-9]+)\\)") :: (String,String,String,[String])
		in [(Rating (array (Normal,Experience) [(Normal,(read n)),(Current,(read c)),(Experience,(read e))]),after)]
	
data SpellImpact=Negative | Positive | Recovery
	deriving (Show,Read,Eq)

data SpellDuration=Permanent | Temporary 
	deriving (Show,Read,Eq)	
	
data Spell=Spell {
	spellName::String,
	impactedChar::Characteristic,
	impact::SpellImpact,
	spellDuration::SpellDuration
	} deriving (Show,Read,Eq)	
	
	
allCharacteristics= [Strength .. Perception]
allHealth=[Physical .. Mental]
			
getDefaultHealth :: CharacteristicRatings -> CharacteristicRatings
getDefaultHealth crs=array (Strength,Mental) ((assocs crs) ++ [(Physical,(mkRating $ getCharacteristic crs Normal Constitution)),(Mental,(mkRating $ getCharacteristic crs Normal Balance))])

isOK :: Character -> Bool
isOK a = not (isOutOfService a)

isOutOfService :: Character -> Bool
isOutOfService a = (isDead a) || (isMad a)

isDead :: Character -> Bool
isDead a= (getCharacteristic' a Current Physical) <= 0  

isMad :: Character -> Bool
isMad a= (getCharacteristic' a Current Mental ) <= 0  

characterLevel :: Character -> Int
characterLevel c=(avg $ map (getCharacteristic' c Normal) 
		-- take only 4 best characteristics
		(take 4 (sortBy (comparing (\ch-> (-(getCharacteristic' c Normal ch)))) allCharacteristics)))
	+ (div (length $ spells c) 3) -- ponder with spells
	+ (div (length $ listCarriedItems $ inventory c) 3) -- ponder with items
	- (if (length $ listActiveItems $ inventory c) == 0 then 2 else 0) -- nothing active is a huge problem
	

mkRating :: Int -> Rating
mkRating v = Rating (array (Normal,Experience) [(Normal,v),(Current, v),(Experience, 0)])

getR :: RatingScoreType -> Rating -> Int
getR rst (Rating array)= array ! rst

addCharacteristic :: CharacteristicRatings -> RatingScoreType -> Characteristic -> Int -> CharacteristicRatings
addCharacteristic cr rst char inc= let 
	(Rating arr)=cr ! char
	oldVal=arr ! rst
	arr2=(if (rst==Experience) 
		then nextLevel
		else id) (arr // [(rst,max 0 (oldVal+inc))])
	in cr // [(char,Rating arr2)]

setCharacteristic :: CharacteristicRatings -> RatingScoreType -> Characteristic -> Int -> CharacteristicRatings
setCharacteristic cr rst char inc= let 
	(Rating arr)=cr ! char
	arr2=(if (rst==Experience) 
		then nextLevel 
		else id) (arr // [(rst,inc)])
	in cr // [(char,Rating arr2)]

nextLevel :: (Array RatingScoreType Int) -> (Array RatingScoreType Int)
nextLevel arr = let
	experience=arr ! Experience
	normal=arr ! Normal
	nextLevelRequired=round (((fromIntegral normal) ** 3) / 3)
	in if (experience>nextLevelRequired)
		then
			let 
				experience'=experience-nextLevelRequired
				normal'=normal+1
				current=(arr ! Current) +1
				arr2=array (Normal,Experience) [(Normal,normal'),(Current, current),(Experience, experience')]
			in nextLevel arr2 	
		else
			arr

getCharacteristic :: CharacteristicRatings -> RatingScoreType -> Characteristic -> Int
getCharacteristic cr rst char = getR rst $ cr ! char

addCharacteristic' :: Character -> RatingScoreType -> Characteristic -> Int -> Character
addCharacteristic' c rst char inc= c{traits=addCharacteristic (traits c) rst char inc}

setCharacteristic' :: Character -> RatingScoreType -> Characteristic -> Int -> Character
setCharacteristic' c rst char inc= c{traits=setCharacteristic (traits c) rst char inc}

getCharacteristic' :: Character -> RatingScoreType -> Characteristic -> Int
getCharacteristic' c = getCharacteristic (traits c)

getCurrentPercentOfNormal :: Character -> Characteristic -> Int
getCurrentPercentOfNormal c char=let
	current=getCharacteristic' c Current char
	normal=getCharacteristic' c Normal char
	in (current * 100) `div` normal

getHealthSummary :: Character -> [String]
getHealthSummary c= map (\x->printf "%s: %s" (show x) (show ((traits c) ! x))) allHealth

addCharacterGold :: Character -> Gold -> Character
addCharacterGold c@(Character{inventory=inv}) g=c{inventory=addGold inv g}

-- | add an affect to the character
addAffect :: Character -- ^ the character to affect
	-> Affect -- ^ the affect
	-> Character -- ^ the new character
addAffect c@(Character{affects=a}) aff=let
	c1=addCharacteristic' c Current (affected aff) (affect aff)
	in c1{affects=(aff:a)}

-- | add an affect to the character
removeAffect :: Character -- ^ the character to affect
	-> Affect -- ^ the affect
	-> Character -- ^ the new character
removeAffect c aff=let
	c1=addCharacteristic' c Current (affected aff) (-(affect aff))
	in c1

-- | remove any expired affects on the character
expireAffects:: Character -- ^ the character to test
	-> Int  -- ^ the current tick count
	-> (Character,[String]) -- ^ the new character and the description of lifted effects
expireAffects c@(Character{affects=a}) tc=let
	(a',a'')=partition (\x->(untilTick x)>tc) a
	c1=foldl removeAffect c a''
	in (c1{affects=a'},map liftDescription a'')
	
	
	
recover :: (MonadWriter ScreenMessages m) => Character
	-> Characteristic
	-> Int
	-> m (Character,Int)
recover c ch n=do
	let
		current = getCharacteristic' c Current ch
		normal = getCharacteristic' c Normal ch
		updated = min normal (current + n)
		diff = updated-current
	when (diff>0) (addScreenMessage (printf "You recover %s!" (points ch diff)))
	return (setCharacteristic' c Current ch updated,(updated-current))
	
	
reflective :: Character -> String
reflective c=case gender c of
	Male -> "himself"
	Female -> "herself"
	
possessive :: Character -> String
possessive c=case gender c of
	Male -> "his"
	Female -> "her"
	
points :: Characteristic -> Int -> String
points c diff=let
	end=if diff==1 
			then "point"
			else "points"
	in (printf ("%d %s " ++ end) diff (show c))	
	
restoreWithTime :: Character -> Int -> Int -> Character
restoreWithTime c tickCount toAdd= restoreWithTimeC (restoreWithTimeC c Physical Constitution tickCount toAdd) Mental Balance tickCount toAdd
	
restoreWithTimeC :: Character -> Characteristic -> Characteristic -> Int -> Int -> Character
restoreWithTimeC c ch1 ch2 tickCount tickToAdd=let 
	current = getCharacteristic' c Current ch1
	normal = getCharacteristic' c Normal ch1
	in if (current>0 && current<normal) 
		then let
			current2=getCharacteristic' c Current ch2
			val=(max 1 (25-current2)) * 20
			prev=div tickCount val
			next=div (tickCount+tickToAdd) val
			toAdd=min (normal-current) (next-prev)
			in addCharacteristic' c Current ch1 toAdd
		else c
	