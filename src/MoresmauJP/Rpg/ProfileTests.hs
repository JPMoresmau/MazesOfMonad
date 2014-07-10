-- | Profiles and character creation hunit tests
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.ProfileTests where

import Data.Maybe

import MoresmauJP.Rpg.Character
import MoresmauJP.Rpg.CharacterTests
import MoresmauJP.Rpg.Profile

import MoresmauJP.Util.Random

import Test.HUnit

import System.Random



profileTests=TestList (map (\p -> mkTest ("testCharacterRS" ++ (fst p)) (testCharacterRS (fst p)) ) profiles)

mkTest s f = TestLabel s (TestCase f)

testCharacterRS pName= do
	sg<-getStdGen
	mt<-evalRandT (generateTraits $ fromJust $ profileByName pName) (ProductionRandom sg)
	let (p,_)=firstProfileAndRatio mt
	assertEqual ("First Profile is "++(fst p)++" instead of "++pName) pName (fst p)
	let c=Character "Test" Male (getDefaultHealth mt) mkEmptyInventory [] []
	let s=show c
	let c2=(read s)::Character
	assertBool "Reading character from show did not give same" ((show c2)==s)