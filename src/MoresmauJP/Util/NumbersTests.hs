-- | Numeric utilities hunit tests
-- (c) JP Moresmau 2009
module MoresmauJP.Util.NumbersTests where

import MoresmauJP.Util.Numbers
import Test.HUnit

numbersTests=TestList [testAverage,testBindInt]

testAverage=TestLabel "testAverage" (TestCase (do
	let ls=[1,2,3]
	assertEqual "average is not 2" 2 (avg ls)
	))
	
testBindInt=TestLabel "testBindInt" (TestCase (do
	assertEqual "bind low is not 0" 1 (bindInt (1,19) 0)
	assertEqual "bind normal is not 10" 10 (bindInt (1,19) 10)
	assertEqual "bind high is not 19" 19 (bindInt (1,19) 20)
	))