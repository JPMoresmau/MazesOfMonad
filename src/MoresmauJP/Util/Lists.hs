-- | List utilities
-- (c) JP Moresmau 2009
module MoresmauJP.Util.Lists 
where

import Control.Monad
import MoresmauJP.Util.Random
import Data.Either
import Data.List

tokenize :: (Eq a) => a -> [a] -> [[a]]
tokenize a l = tokenizeGen (\x->x==a) l

tokenizeGen :: (a->Bool) -> [a] -> [[a]]
tokenizeGen f l = tokenize' f l []

tokenize' _ [] ss = reverse ss
tokenize' f s ss = 
	let 
		(s1,s2) = break f s
	in tokenize' f (safeTail s2) (s1:ss)
	
safeTail [] = []
safeTail (_:ss) = ss

maximumIndex :: (Ord a) => [a] -> Int
maximumIndex []=error "maximumIndex on empty list"
maximumIndex (x:xs) = snd (maximumIndex' xs (x,0) 1)

maximumIndex' :: (Ord a) => [a] -> (a,Int) -> Int -> (a,Int)
maximumIndex' [] (m,mi) _ = (m,mi)
maximumIndex' (x:xs) (m,mi) ix= 
	let (m',mi')=
		case x>m of
			True -> (x, ix)
			_  -> (m, mi)
	in maximumIndex' xs (m',mi') (ix+1)

occurenceList :: [(a,Int)] -> [a]
occurenceList itemOccurences=concat (foldl (\l (it,oc)->(take oc (repeat it)):l) [] itemOccurences)
	
randomPickp:: (MonadRandom m)=> [a] -> m a
randomPickp [] = error "randomPickp cannot run on an empty list" 
randomPickp [x] = return x
randomPickp (x:xs) = pick' x xs 2
	where 
		pick' :: (MonadRandom m)=> a -> [a] -> Int -> m a
		pick' curr [] _= return curr
		pick' curr (x:xs) prob= do
			r<-getRandomRange (1,prob)
			let curr' = if r==1 
				then x 
				else curr
			pick' curr' xs (prob+1)

randomPickpn:: (MonadRandom m,Eq a)=> [a] -> Int -> m [a]
randomPickpn _ 0= return [] 
randomPickpn [] _= return []
randomPickpn l n= pick [] 
	where 
	realLength=min (length l) n
	pick l2 =
		if	(length l2)==realLength	
			then return l2
			else do
				x<- randomPickp l
				pick $ nub (x:l2)

randomHeadp :: (MonadRandom m)=> [a] -> m [a]
randomHeadp [] = return []
randomHeadp [x] = return [x]
randomHeadp l =
	do
		newIdx<-getRandomRange (0,(length l)-1)
		let (l1,x:l2) = splitAt newIdx l 
		return (x:(l1++l2))
		
	
(//) :: [a] -> Int -> a -> [a]
(//) xs ix newel= let
	(xs1,xs2)=splitAt ix xs
	in xs1++(newel:(tail xs2))
	
imap :: [a] -> Int -> (a->a) -> [a]
imap xs ix f= let
	(xs1,(oldEl:xs2))=splitAt ix xs
	in xs1++((f oldEl):xs2)
	
swap :: (a,b) -> (b,a)
swap (a,b)=(b,a)

eitherConcat :: [Either [a] [a]] -> Either [a] [a]
eitherConcat l=foldl eitherConcat' (Right []) l

eitherConcat' (Right a) (Right b)=Right (a++b)
eitherConcat' (Left a) (Right b)=Left (a++b)
eitherConcat' (Right a) (Left b)=Left (a++b)
eitherConcat' (Left a) (Left b)=Left (a++b) 

isRight (Right _)=True
isRight (Left _)=False

