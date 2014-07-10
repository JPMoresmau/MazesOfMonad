-- | Profiles and character creation
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.Profile where

import Data.Array.IArray
import Control.Monad.Writer
import Data.List
import Data.Maybe

import MoresmauJP.Core.Screen
import MoresmauJP.Rpg.Actions
import MoresmauJP.Rpg.Character
import MoresmauJP.Rpg.Inventory
import MoresmauJP.Rpg.Magic
import MoresmauJP.Util.Lists  hiding ((//))
import MoresmauJP.Util.Random


profiles :: [Profile]
profiles = [("Warrior",[VeryHigh Strength,High Dexterity, High Constitution])
	,("Thief",[VeryHigh Dexterity,NotTooLow Intelligence,High Perception])
	,("Merchant",[VeryHigh Charisma, High Intelligence, NotTooLow Willpower])
	,("Wizard",[VeryHigh Intelligence, High Willpower, High Balance])
	,("Priest",[High Charisma, VeryHigh Willpower, NotTooLow Balance])
	,("Ranger",[VeryHigh Constitution, High Dexterity, VeryHigh Perception, NotTooLow Strength, NotTooLow Balance])
	]

profileNames :: [String]
profileNames = map fst profiles

type Profile = (String,[Criteria])

profileByName :: String -> Maybe Profile
profileByName s= listToMaybe (filter ((== s).fst) profiles)


data Criteria = High { 
		func::Characteristic
	}
	| VeryHigh  { 
		func::Characteristic
	}
	| NotTooLow  { 
		func::Characteristic
	}

generateCharacter::  (MonadRandom m,MonadWriter ScreenMessages m)=>Name -> Gender -> Profile -> m Character
generateCharacter name gender prof=do
	mt<-generateTraits prof
	let h=getDefaultHealth mt
	let c=Character name gender h (makeEmptyInventory 10 20) [] []
	extraGoldFromTrade<-diffResult' c trade (subsequentDifficulty Standard) (\x->if x<0 then 0 else x ^ 2)
	extraGoldFromStealing<-diffResult' c steal (subsequentDifficulty Standard) (\x->if x<0 then 0 else x ^ 2)
	let extraGold=max extraGoldFromTrade extraGoldFromStealing
	nbSpellsLearned<-diffResult' c spelllearning (toIntLevel Neutral) (\x->if x<0 then 0 else (round $ sqrt $ fromIntegral x))
	spells<-randomPickpn allSpells nbSpellsLearned
	let c'=c{inventory=addGold (inventory c) extraGold,spells=spells} 
	return c'

generateTraits :: (MonadRandom m)=>Profile -> m CharacteristicRatings
generateTraits p=do
	let mt=array (head allCharacteristics,last allCharacteristics) (map (\x->(x,(mkRating 3))) allCharacteristics)
	mt1<-makeFit mt p (8*(length allCharacteristics))
	let (n,_)=firstProfileAndRatio mt1
	if (fst p)==(fst n)
		then
			return mt1
		else
			generateTraits p
	

makeFit :: (MonadRandom m)=>CharacteristicRatings-> Profile -> Int -> m CharacteristicRatings
makeFit mt _ 0 = return (mt)
makeFit mt p t = do
	mt1<-makeFitter mt p
	makeFit mt1 p (t-1)

makeFitter :: (MonadRandom m)=>CharacteristicRatings -> Profile -> m CharacteristicRatings
makeFitter mt (n,cs) = do
	let modifiableCs=filter ((isRatingBelowMax mt).func) cs
	let vals=(map (\x -> (x,traitsScore mt x)) modifiableCs)
	let allZeros=filter (\(_,f)->f==0) vals
	rf<-if not (null allZeros) 
		then
			return (func $ fst $ (head allZeros))
		else
			do 
				let (p,_)=firstProfileAndRatio mt
				if (fst p)==n 
					then
						randomPickp (filter (isRatingBelowMax mt) allCharacteristics)
					else
						return (func $ fst $ minimumBy (\a b->compare (snd a) (snd b)) vals)
	let mt1=addCharacteristic mt Current rf 1
	return (addCharacteristic mt1 Normal rf 1 )	
	
isRatingBelowMax :: CharacteristicRatings -> Characteristic -> Bool
isRatingBelowMax mt c=(getCharacteristic mt Current c)<17

firstProfileAndRatio:: CharacteristicRatings -> (Profile,Float)
firstProfileAndRatio mt=foldr1 maxRatio (allProfilesAndRatios mt)

bestProfiles :: CharacteristicRatings -> [(Profile,Float)]
bestProfiles mt= let
	aboveAverage=filter (\(_,f)->f>=0.5) (allProfilesAndRatios mt)
	in 
		if null aboveAverage
			then [firstProfileAndRatio mt]
			else aboveAverage

maxRatio :: (Profile,Float) -> (Profile,Float) -> (Profile,Float)
maxRatio (p1,f1) (p2,f2)= if f1>f2
	then (p1,f1)
	else (p2,f2)

allProfilesAndRatios:: CharacteristicRatings -> [(Profile,Float)]
allProfilesAndRatios mt = map (\p@(_,cs)->(p,traitsScores mt cs)) profiles
	
traitsScores :: CharacteristicRatings -> [Criteria] -> Float
traitsScores mt cs = combineScores $  map (traitsScore mt) cs
	
traitsScore :: CharacteristicRatings -> Criteria -> Float
traitsScore mt c = ratingScore (getCharacteristic mt Current (func c)) c

ratingScore :: Int -> Criteria -> Float
ratingScore v (High _)
	| v<10 = 0
	| otherwise = sqrt ((fromIntegral (v-10)) / 10)
ratingScore v (VeryHigh _)
	| v<12 = 0
	| otherwise = sqrt $ sqrt ((fromIntegral (v-12)) / 8)
ratingScore v (NotTooLow _)
	| v<8 = 0
	| otherwise = ((fromIntegral (v-8)) / 12)
	
combineScores :: [Float] -> Float
combineScores = minimum

