-- | Action resolution, some kind of D20 system
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.Actions where

import Control.Monad.Writer

import MoresmauJP.Core.Screen
import MoresmauJP.Rpg.Character
import MoresmauJP.Util.Numbers
import MoresmauJP.Util.Random

import Text.Printf

initiative = [Dexterity,Willpower]
melee = [Strength,Dexterity]
--archery = [Dexterity,Perception,Strength]
conversion=[Charisma,Willpower,Willpower]
trade=[Charisma,Charisma,Intelligence]
spellcasting=[Intelligence,Willpower]
spelllearning=[Intelligence]
escape=[Dexterity]
medecine=[Intelligence,Intelligence,Dexterity,Perception]
pray=[Charisma,Willpower]
detectTrap=[Perception,Perception,Intelligence]
--disableTrap=[Dexterity,Perception]
steal=[Dexterity,Dexterity,Intelligence,Perception,Perception]

d20=(1,20)

roll :: (MonadRandom m) => (Int,Int) -> m Int
roll (low,high)= getRandomRange (low,high)

rolls :: (MonadRandom m) => Int -> (Int,Int) -> m [Int]
rolls a (low,high)= replicateM a (roll (low,high)) 

action :: (MonadRandom m,MonadWriter ScreenMessages m) => Character -> [Characteristic] -> Difficulty -> m (Character,RollResult)	
action c cs d= do
	r <- roll d20
	processAction c cs d r

actionNoExperience :: (MonadRandom m,MonadWriter ScreenMessages m) => Character -> [Characteristic] -> Difficulty -> m RollResult	
actionNoExperience c cs d=liftM snd (action c cs d)

diffResult :: (MonadRandom m,MonadWriter ScreenMessages m) => Character -> [Characteristic] -> Difficulty -> m Int
diffResult c cs d= do
	r <- roll d20
	let avgCs=score c cs
	let myScore=avgCs+d
	return (myScore-r)

diffResult' :: (MonadRandom m,MonadWriter ScreenMessages m) => Character -> [Characteristic] -> Difficulty -> (Int -> Int) -> m Int	
diffResult' c cs d f=do
	diff<-diffResult c cs d
	let diff'=f diff
	return 
		(if (diff<0)
			then (-diff')
			else diff')

competeWithDiff :: (MonadRandom m,MonadWriter ScreenMessages m) => Character -> Character-> [Characteristic] -> Int -> m ((Character,Character),RollResult)
competeWithDiff c1 c2 cs d=do
	r <- roll d20
	processCompeteDiff c1 c2 cs r d
	
compete :: (MonadRandom m,MonadWriter ScreenMessages m) => Character -> Character-> [Characteristic] -> m ((Character,Character),RollResult)
compete c1 c2 cs=competeWithDiff c1 c2 cs 0
	
processCompete :: (MonadWriter ScreenMessages m) => Character -> Character-> [Characteristic] -> Int -> m ((Character,Character),RollResult)
processCompete c1 c2 cs r =processCompeteDiff c1 c2 cs r 0

processCompeteDiff :: (MonadWriter ScreenMessages m) => Character -> Character-> [Characteristic] -> Int -> Int -> m ((Character,Character),RollResult)
processCompeteDiff c1 c2 cs r di= do
	let
		avgCs1=score c1 cs
		avgCs2=score c2 cs
		d=(div (avgCs1-avgCs2) 2)+di
	(c1b,rr1) <- (processAction c1 cs d r)
	(c2b,_) <- (processAction c2 cs (-d) r)
	return ((c1b,c2b),rr1)
	
score :: Character -> [Characteristic] -> Int
score c cs = avg (map (getCharacteristic' c Current) cs)	
	
processAction :: (MonadWriter ScreenMessages m) => Character-> [Characteristic] -> Difficulty -> Int -> m (Character,RollResult)	
processAction c cs d roll= do
	let 
		avgCs=score c cs
		myScore=bindInt (1,20) (avgCs+d)
		(rr,em)=evalResult roll myScore
		expGain=experienceGain myScore em (length cs)
		previousNormal=map (getCharacteristic' c Normal) allCharacteristics
		c3=foldr (\b c2->addCharacteristic' c2 Experience b expGain) c cs
		newNormal=map (getCharacteristic' c3 Normal) allCharacteristics
		augmented=filter (\(_,b)->b>0) $map (\(a,b,c)->(a,c-b)) $ zip3 allCharacteristics previousNormal newNormal
	mapM_ (\(a,b)->addScreenMessage (printf (msg b) (name c3) b (show a))) augmented
	return (c3,rr)
	where 
		msg 1="%s gains %d point in %s"
		msg _="%s gains %d points in %s"	

experienceGain :: Int -> Int -> Int -> Int
experienceGain score expM l= div ((max 1 (div ((20-score)^2) 10)) * expM) l
	
evalResult:: Int -> Int -> RollResultExp
evalResult roll score
	| roll==20                    = (Failure Exceptional (max 2 (roll-score)),1)
	| roll==1                     = (Success Exceptional (max 2 (score-roll)),1)
	| (roll < (div score 5)) 	  = (Success Exceptional (score-roll),4)
	| (roll < (div score 2)) 	  = (Success Remarkable (score-roll),3)
	| (roll <= score)             = (Success Standard (score-roll),2)
	| (roll > (20-(div score 5))) = (Failure Exceptional (roll-score),1)
	| (roll > (20-(div score 2))) = (Failure Remarkable (roll-score),1)
	| otherwise  = (Failure Standard (roll-score),1)


type Difficulty=Int

type RollResultExp=(RollResult,ExperienceMultiplier)
type ExperienceMultiplier=Int

data DifficultyLevel=NearImpossible | VeryHard | Hard | RatherHard | Neutral 
	| RatherEasy | Easy | VeryEasy | NearUnmissable
	deriving (Eq,Show,Read,Ord,Bounded,Enum)

toIntLevel :: DifficultyLevel -> Difficulty
toIntLevel level =3 * ((fromEnum level) - 4)

data RollResult = 
	Failure {
		grade::Grade,
		diff::Int} 
	| Success {
		grade::Grade,
		diff:: Int} 
	deriving (Show, Eq, Read)

isSuccess :: RollResult -> Bool
isSuccess (Success {})=True
isSuccess _ =False

subsequentDifficulty:: Grade -> Difficulty
subsequentDifficulty Standard=0
subsequentDifficulty Remarkable=2
subsequentDifficulty Exceptional=6

resultMultiplier :: Grade -> Int -> Float
resultMultiplier Standard i= fromIntegral i * 2
resultMultiplier Remarkable i= fromIntegral i * 4
resultMultiplier Exceptional i= fromIntegral i * 10

{--
resultMultiplierSc :: Grade -> Int -> Float
resultMultiplierSc Standard i= fromIntegral $ div i 4 
resultMultiplierSc Remarkable i= fromIntegral $ div i 2 
resultMultiplierSc Exceptional i= fromIntegral i

resultMultiplierScale :: Int -> RollResult -> Int
resultMultiplierScale i (Failure gr d)=max 0 (i  - (round $ resultMultiplierSc gr (abs d)))
resultMultiplierScale i (Success gr d)=i + (round $ resultMultiplierSc gr d)
--}

resultMultiplierHigh :: Int -> RollResult -> Int
resultMultiplierHigh i (Failure gr d)=max 0 (i  - round((fromIntegral i * (resultMultiplier gr d))/100))
resultMultiplierHigh i (Success gr d)=i + round ((fromIntegral i * (resultMultiplier gr d))/100)

resultMultiplierLow :: Int -> RollResult -> Int
resultMultiplierLow i (Success gr d)=max 0 (i  - round((fromIntegral i * (resultMultiplier gr d))/100))
resultMultiplierLow i (Failure gr d)=i + round ((fromIntegral i * (resultMultiplier gr d))/100)

resultExtra :: Int -> RollResult -> Int
resultExtra i (Failure {})=i
resultExtra i rr=resultMultiplierHigh i rr

data Grade =
	Standard
	| Remarkable
	| Exceptional
	deriving (Show,Enum,Read,Eq)

	
testRM = do
	mapM (s 10) [(Success x y) | x<-[Standard .. Exceptional],y<-[1,5,10,15,20]]
	where s i rr=do
		putStrLn (printf "High: %d %s -> %d" i (show rr) (resultMultiplierHigh i rr))
		putStrLn (printf "Low: %d %s -> %d" i (show rr) (resultMultiplierLow i rr))
		--putStrLn (printf "Scale: %d %s -> %d" i (show rr) (resultMultiplierScale i rr))