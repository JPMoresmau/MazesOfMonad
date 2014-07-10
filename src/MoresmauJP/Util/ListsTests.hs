-- | List utilites hunit tests
-- (c) JP Moresmau 2009
module MoresmauJP.Util.ListsTests where

import Control.Monad.Identity

import MoresmauJP.Util.Lists
import MoresmauJP.Util.Random

import Test.HUnit

listsTests =TestList [testRandomPickp,testRandomPickpn]

	
testRandomPickp=TestLabel "Test RandomPick" (TestCase (do
	let rw=mkTestWrapper [2]
	let r=runIdentity $ evalRandT (randomPickp [4,5,6,7]) rw
	assertEqual "r is not first" 4 r
	let rw=mkTestWrapper [1]
	let r=runIdentity $ evalRandT (randomPickp [4,5,6,7]) rw
	assertEqual "r is not last" 7 r
	let rw=mkTestWrapper [1,2,2,2]
	let r=runIdentity $ evalRandT (randomPickp [4,5,6,7]) rw
	assertEqual "r is not second" 5 r
	))	

testRandomPickpn=TestLabel "Test RandomPickn" (TestCase (do
	let rw=mkTestWrapper [2]
	let r=runIdentity $ evalRandT (randomPickpn [4,5,6,7] 1) rw
	assertEqual "r is not first" [4] r
	let rw=mkTestWrapper [1]
	let r=runIdentity $ evalRandT (randomPickpn [4,5,6,7] 1) rw
	assertEqual "r is not last" [7] r
	let rw=mkTestWrapper [1,2,2,2]
	let r=runIdentity $ evalRandT (randomPickpn [4,5,6,7] 1) rw
	assertEqual "r is not second" [5] r
	let rw=mkTestWrapper [2,2,2,1,2,2]
	let r=runIdentity $ evalRandT (randomPickpn [4,5,6,7] 2) rw
	assertEqual "r is not 5,4" [5,4] r
	let rw=mkTestWrapper [2,2,1,2,2,2]
	let r=runIdentity $ evalRandT (randomPickpn [4,5,6,7] 2) rw
	assertEqual "r is not 4,7" [4,7] r
	))	