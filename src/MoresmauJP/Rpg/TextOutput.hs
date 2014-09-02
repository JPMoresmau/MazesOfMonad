-- | Pretty printing to text output
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.TextOutput where

import Data.Array.IArray
import Data.List

import MoresmauJP.Rpg.Character
import MoresmauJP.Rpg.Inventory
import MoresmauJP.Rpg.NPC
import MoresmauJP.Rpg.Profile
import MoresmauJP.Rpg.Trade

import Text.PrettyPrint
import Text.Printf

ppCharacter' :: Character -> String
ppCharacter'= render . ppCharacter

ppCharacterAndInventory' :: Character -> String
ppCharacterAndInventory' c = render ((ppCharacter c) $$ (text "---") $$ (ppInventory $ inventory c))

ppCharacterAndGold' :: Character -> String
ppCharacterAndGold' c = render ((ppCharacter c) $$ (text "---") $$ (ppGold $ getGold $ inventory c))



ppCharacter :: Character -> Doc
ppCharacter c= 
	(text $ name c) 
		$$ (text $ show $ gender c)
		$$ (text "---")
		$$ (ppTraits c)
		$$ (text "---")
		$$ (ppSpells c)
		$$ (text "---")
		$$ (ppProfiles c)
		$$ (ppAffects c) 
		$$ (ppStatus c)

ppTraits :: Character -> Doc
ppTraits c= vcat (map f (assocs $ traits c))
	where 
		f (char,r)=(text $ show char) $$ (nest (m+2) (text $ show r)) $$ (nest (m+15) (
			case char of
				Physical -> text $ ppPhysical c
				Mental -> text $ ppMental c
				_ -> empty
			))
		m = maximum (map (length . show) (allCharacteristics++allHealth))
		
ppProfiles :: Character -> Doc
ppProfiles c= (text "Profile(s): " <+> (vcat (map (text.fst.fst) (bestProfiles (traits c)))))

ppStatus :: Character -> Doc
ppStatus c 
		| (isDead c) = text "Dead!"
		| (isMad c) = text "Mad!"
		| otherwise = empty

ppAffects :: Character -> Doc
ppAffects c=(vcat (map (text . affectDescription) (affects c)))

ppSpells :: Character -> Doc
ppSpells c=text "Spell(s) known: " <+> (vcat (map (text . spellName) (spells c)))

ppInventory' :: Inventory -> String
ppInventory'= render . ppInventory


ppInventory :: Inventory -> Doc
ppInventory i=(text "Inventory:") $$ (ppInventoryItems $listItems i ) $$ (ppGold $ getGold i) 

ppGold :: Gold -> Doc
ppGold 0=text "No gold"
ppGold 1=text "1 gold coin"
ppGold n=(text $ show n) <+> (text "gold coins")

ppInventoryItems :: [(Position,Maybe ItemType)] -> Doc
ppInventoryItems items=(vcat (map f items))
	where
		m = foldl max 0 (map (length . show . fst) items)
		f (pos,maybeItem)=(text $ show pos) $$ (nest (m+2) (ppMaybeItem maybeItem))
		 
ppInventoryPosition' :: (Position,Maybe ItemType) -> String
ppInventoryPosition' (pos,maybeItem)= render $ (text $ show pos) <+> (f maybeItem)		
		 where 
		 	f Nothing=empty
		 	f (Just it)=parens$ text $ itName it
		 
ppMaybeItem :: Maybe ItemType -> Doc
ppMaybeItem Nothing = text "-"
ppMaybeItem (Just it) = text $ itName it
		 
ppItemPosition :: (Position,ItemType) -> Doc
ppItemPosition (pos,item) =(text $ itName item) <+> (parens (text $ show pos))
	
ppItemPosition' :: (Position,ItemType) -> String
ppItemPosition' = render . ppItemPosition
		
ppTradeOperation :: TradeOperation -> Doc
ppTradeOperation (ToBuy (_,it) g)=text (printf "Buy a %s for %d gold coins" (itName it) g) 
ppTradeOperation (ToSell (pos,it) g)=text (printf "Sell a %s (%s) for %d gold coins" (itName it) (show pos) g)
ppTradeOperation (ToExchange (pos,it1) (_,it2))= text (printf "Exchange a %s (%s) for a %s" (itName it1) (show pos) (itName it2))
		
ppTradeOperation' :: TradeOperation -> String
ppTradeOperation' = render . ppTradeOperation	

ppAttitude :: NPCCharacter -> String
ppAttitude (NPCCharacter{npcAttitude=a}) 
	| a<5="very hostile"
	| a<9="hostile"
	| a<13="neutral"
	| a<17="friendly"
	| otherwise="very friendly"
	
ppPhysical :: Character -> String
ppPhysical c=ppPhysical' $ getCurrentPercentOfNormal c Physical
	where 
		ppPhysical' :: Int -> String
		ppPhysical' a 
			| a<11="nearly dead"
			| a<26="very seriously wounded"
			| a<51="badly wounded"
			| a<75="injured"
			| a<100="bruised and scratched"
			| otherwise="in perfect health"
	
ppMental :: Character -> String
ppMental c=ppPhysical' $ getCurrentPercentOfNormal c Mental
	where 
		ppPhysical' :: Int -> String
		ppPhysical' a 
			| a<11="verging on the totally insane"
			| a<26="rambling"
			| a<51="chaotic"
			| a<75="haggard"
			| a<100="a bit agitated"
			| otherwise="fully sane"	
					
ppNPC :: NPCCharacter -> String
ppNPC npc=let
	c=npcCharacter npc
	in printf "a %s, looking %s and apparently %s and %s" (name c) (ppAttitude npc) (ppPhysical c) (ppMental c)
 
