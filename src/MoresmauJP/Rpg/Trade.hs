-- | Trade resolution
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.Trade where

import Data.List
import Data.Maybe
import Data.Ord

import MoresmauJP.Rpg.Actions
import MoresmauJP.Rpg.Character
import MoresmauJP.Rpg.Inventory


data TradeAction=Buy | Sell | Exchange | EndTrade	
	deriving (Show,Read,Enum,Eq,Bounded)
	
data TradeOperation= ToBuy (Position,ItemType) Gold | ToSell (Position,ItemType) Gold | ToExchange (Position,ItemType) (Position,ItemType)
	deriving (Show,Read,Eq)

listTradeItems :: Character -> Character -> TradeAction -> RollResult -> [TradeOperation]
listTradeItems (Character{inventory=inv1}) (Character{inventory=inv2}) Buy rr=
	let
		canBuy=listTradeItems' inv1 inv2 rr resultMultiplierLow
	in map (\(a,b)->ToBuy a b) canBuy
listTradeItems (Character{inventory=inv1}) (Character{inventory=inv2}) Sell rr=
	let
		canBuy=listTradeItems' inv2 inv1 rr resultMultiplierHigh
	in filter (\op->isJust $ getNpcPosition op inv2) $  map (\(a,b)->ToSell a b) canBuy
listTradeItems c1@(Character{inventory=inv1}) c2@(Character{inventory=inv2}) Exchange rr=
	let 
		-- we don't want to restrict to what we could buy/sell with our money
		-- so let's pretend with have maximum money
		buy=listTradeItems c1{inventory=addGold inv1 maxBound} c2 Buy rr
		sell=listTradeItems c1 c2{inventory=addGold inv2 maxBound} Sell rr
		allMaps=map (\t@(ToBuy _ g)->(t,getOperationForPrice sell g)) buy
		possible=filter (\(_,y)->isJust y) allMaps
	in
		map (\((ToBuy i1 _),Just (ToSell i2 _))->ToExchange i2 i1) possible
listTradeItems _ _ _ _=[]
	
getOperationForPrice :: [TradeOperation] -> Gold -> Maybe TradeOperation
getOperationForPrice tos g=
	let 
		within10=filter (\(ToSell _ gold)-> (abs(g-gold))<(div g 5)) tos
		closest=sortBy (\(ToSell _ gold1) (ToSell _ gold2)->compare (gold2-g) (gold1-g)) within10
	in listToMaybe closest
	
listTradeItems' :: Inventory -> Inventory -> RollResult -> (Int -> RollResult -> Int)	-> [((Position,ItemType),Gold)]
listTradeItems' inv1 inv2 rr f=
	let 
		maxGold=getGold inv1
		forSale=map (\a-> (a,valueOfItem rr a f)) (listCarriedItemsUniqueObject inv2)
	in filter (\a->(snd a) <=maxGold) forSale
	
valueOfItem::RollResult -> (Position,ItemType) -> (Int -> RollResult -> Int)-> Gold
valueOfItem rr (pos,it) f= 
	let
		p=price it
		p'=max 1 $ f p rr
	in	positionPremium pos p'

positionPremium :: Position -> Int -> Int
positionPremium (Bag {}) i = i 
positionPremium _  i =div (i * 14) 10

doTradeOperation :: (Character,Maybe Position) -> (Character,Maybe Position) -> TradeOperation -> (Character,Character,[ItemType])
doTradeOperation (c1,Just posTo) (c2,Nothing) (ToBuy (pos,_) g)=
	let
		(i22,i12,dropped)=doMoneyOperation (inventory c2,pos) (inventory c1,posTo) g
	in (c1{inventory=i12},c2{inventory=i22},dropped)
doTradeOperation (c1,Nothing) (c2,Just posTo) (ToSell (pos,_) g)=
	let
		(i12,i22,dropped)=doMoneyOperation (inventory c1,pos) (inventory c2,posTo) g
	in (c1{inventory=i12},c2{inventory=i22},dropped)	
doTradeOperation (c1,Just posc1) (c2,Just posc2) (ToExchange (pos1,_) (pos2,_))=
	let
		Right (invc11,Just it1)=dropItem (inventory c1) pos1
		Right (invc21,Just it2)=dropItem (inventory c2) pos2
		Right (invc12,dropped1)=takeItem invc11 it2 posc1
		Right (invc22,dropped2)=takeItem invc21 it1 posc2
	in  (c1{inventory=invc12},c2{inventory=invc22},dropped1++dropped2)	
doTradeOperation _ _ _=error "doTradeOperation unhandled case"

doMoneyOperation :: (Inventory,Position) -> (Inventory,Position) -> Gold -> (Inventory,Inventory,[ItemType])
doMoneyOperation (invFrom,pFrom) (invTo,pTo) g=
	let
		Right (invFrom1,Just it)=dropItem invFrom pFrom
		invFrom2=addGold invFrom1 g
		Right (invTo1,dropped)=takeItem invTo it pTo
		invTo2=addGold invTo1 (-g)
	in (invFrom2,invTo2,dropped)
	
getNpcPosition :: TradeOperation -> Inventory -> Maybe Position
getNpcPosition (ToBuy _ _) _ = Nothing
getNpcPosition (ToExchange (_,it) (pos,_)) i = 
	let
		Right (i2,_)=dropItem i pos
	in 	getNpcPosition (ToSell (undefined,it) undefined) i2
getNpcPosition (ToSell (_,it) _) i = 
        let
                pos1=listAllowedPositions i it
        in      if null pos1 
                        then
                                Nothing
                        else
                                let
                                (filled,empties)=partition (isJust . snd) pos1
                	        slot=if null empties
                                        then		
                			        minimumBy (\(_,Just y1) (_,Just y2)->comparing price y1 y2) filled
                	                else
                                                head empties		
	                        in Just $ fst slot
