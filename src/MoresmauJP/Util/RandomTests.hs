-- | Random handling utilities hunit tests
-- (c) JP Moresmau 2009
module MoresmauJP.Util.RandomTests where

import Control.Monad.State

import MoresmauJP.Util.Random

import System.Random

import Test.HUnit

randomTests = TestList [
	testConstantRandomW,testListRandomW,testRealRandomW,testMonadState]
	
testConstantRandomW = TestLabel "Test Constant Random Wrapper" (TestCase (do
	let tg=mkTestWrapper [2]
	let l=randomRanges (1,6) tg 3 
	assertEqual ("l is not 3 2s") [2,2,2] (fst l)
	return ()
	))
	
testListRandomW = TestLabel "Test List Random Wrapper" (TestCase (do
	let tg=mkTestWrapper [2,3,4]
	let l=randomRanges (1,6) tg 4 
	assertEqual ("l is not 2,3,4,2") [2,3,4,2] (fst l)
	return ()
	))
	
testRealRandomW = TestLabel "Test Real Random Wrapper" (TestCase (do
	tg<-getStdGen
	let prod=ProductionRandom tg
	let (r,prod')= randomRange (minBound,maxBound) prod 
	let r'=fst $ randomRange (minBound,maxBound) prod' 
	assertBool "same value returned" (r/=r')
	))	
	
testMonadState=TestLabel "Test Monad State" (TestCase (do
	result<-(execStateT (evalRandT (do
		i<-get
		die<-getRandomRange (1,6)
		put (i+die)
		i<-get
		die<-getRandomRange (1,6)
		put (i+die)
		return ""
		) (mkTestWrapper [2,4])) 1)
	assertEqual "result is not 7" 7 result
	))
