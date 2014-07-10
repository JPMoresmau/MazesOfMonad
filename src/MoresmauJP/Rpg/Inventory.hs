-- | Inventory management
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.Inventory (
        Inventory(), Position(..), ItemType(..), InventoryError(..),PotionType(..),
        makeEmptyInventory, makeFullInventory, takeItem, dropItem, listItems, 
        listAllowedPositions,listActiveItems,listCarriedItems,listCarriedItemsUniqueObject,
        getCarriedItem,        positionAllowed, addGold, getGold,
        Gold, isActive
        ) where

import Data.List
import Data.Maybe
import qualified Data.Map as M

data ItemType = Weapon {
                itName::String, 
                damageLow::Int, 
                damageHigh::Int,
                hands::Int,
                price::Gold}
        | Armor {
                itName::String, 
                damageLow::Int, 
                damageHigh::Int,
                price::Gold}
        | Shield {
                itName::String, 
                damageLow::Int, 
                damageHigh::Int,
                price::Gold}
        | Helmet {
                itName::String, 
                damageLow::Int, 
                damageHigh::Int,
                price::Gold}
        | Potion {
                itName::String,
                potionType::PotionType,
                price::Gold}
        | Scroll {
                itName::String,
                spellType::String,
                price::Gold}
        | Trap {
                itName::String,
                damageLow::Int, 
                damageHigh::Int,
                triggerDescription::String
                }
        deriving (Show,Read,Eq)
        

data Position= RightHand | LeftHand | Body | Head | Bag Int
        deriving (Show,Read,Eq,Ord)

-- | Inventory Map item by Position Bag Size 
data Inventory = Inventory {
        invItems::(M.Map Position ItemType),
        invBagCapacity:: Int,
        invGold:: Gold
        } deriving (Eq,Show,Read)

type Gold=Int

data InventoryError = InvalidBagPosition Int
        | InvalidPositionForItem ItemType Position
        deriving (Eq,Show,Read)

data PotionType = HealingPotion Int | MindPotion Int
        deriving (Show,Read,Eq)

makeEmptyInventory :: Int -> Gold -> Inventory
makeEmptyInventory a gold= Inventory M.empty a gold

{--
makeFullInventory :: [(Position,ItemType)] -> Gold -> Inventory
makeFullInventory [] gold= makeEmptyInventory 0 gold
makeFullInventory posAndItems gold= let
        maxPos=maximum (map (getBagPos . fst) posAndItems)
        in
                reconcile2Hand (Inventory M.empty maxPos gold) posAndItems
        where
                getBagPos (Bag p)=p
                getBagPos _ =0
--}

makeFullInventory :: [(Position,ItemType)] -> Int -> Gold -> Inventory
makeFullInventory [] maxPos gold= makeEmptyInventory maxPos gold
makeFullInventory posAndItems maxPos gold= reconcile2Hand (Inventory M.empty maxPos gold) posAndItems

addGold :: Inventory -> Gold -> Inventory
addGold i g=let        oldGold=invGold i
        in i{invGold=max 0 (oldGold+g)}

getGold :: Inventory -> Gold
getGold i=invGold i

reconcile2Hand :: Inventory -> [(Position,ItemType)] -> Inventory
reconcile2Hand i poss=foldr f i poss
        where 
                f:: (Position,ItemType) -> Inventory -> Inventory
                f pos i=
                        case getCarriedItem i (fst pos)        of
                                Right Nothing ->
                                        let Right (i2,_)=takeItem i (snd pos) (fst pos)
                                        in i2
                                _->i

takeItem :: Inventory -> ItemType -> Position -> Either InventoryError (Inventory,[ItemType])
takeItem i@(Inventory {invBagCapacity= sz}) it p=  do
        p2 <- checkPos sz p
        ti1 <- takeItem' i it p2 True
        takeTwoHandsItem ti1 it p2
        
takeTwoHandsItem ::(Inventory,Maybe ItemType) -> ItemType -> Position  -> Either InventoryError (Inventory,[ItemType])
takeTwoHandsItem (i,it1) it@(Weapon {hands=2}) RightHand= do
        (iv2,it2) <- takeItem' i it LeftHand (needSndDrop it1)
        return (iv2,catMaybes [it1,it2])
takeTwoHandsItem (i,it1) it@(Weapon {hands=2}) LeftHand= do
        (iv2,it2) <- takeItem' i it RightHand (needSndDrop it1)
        return (iv2,catMaybes [it1,it2])
takeTwoHandsItem (i,it1)  _ _=do
        return (i,catMaybes [it1])
        
needSndDrop :: Maybe ItemType -> Bool
needSndDrop (Just (Weapon {hands=2}))=False
needSndDrop _=True

takeItem' :: Inventory -> ItemType -> Position -> Bool -> Either InventoryError (Inventory,Maybe ItemType)
takeItem' i it p needDrop=  do
        let allowed=positionAllowed it p
        if allowed 
                then do
                        (i2@(Inventory {invItems=m}),it2) <- if needDrop 
                                then dropItemToBag i p
                                else Right (i, Nothing)
                        return ((i2{invItems=(M.insert p it m)}),it2)
                else Left (InvalidPositionForItem it p)

dropItemToBag :: Inventory -> Position -> Either InventoryError (Inventory,Maybe ItemType)
dropItemToBag i p@(Bag _)=dropItem i p
dropItemToBag i p=do
        mi2@(i2,it2)<- dropItem i p
        if isJust it2
                then do
                        let freeBagPos=getFirstFreeBagPos i2
                        if (isJust freeBagPos)
                                then do
                                        (i3,_)<- takeItem i2 (fromJust it2) (fromJust freeBagPos)
                                        return (i3,Nothing)
                                else 
                                        return mi2
                else return mi2

--instance Monad (Either InventoryError) where
--        Left a >>= _ = Left a
--        Right a >>= f = f a
--        return = Right 

getFirstFreeBagPos:: Inventory -> Maybe Position
getFirstFreeBagPos (Inventory {invItems=m,invBagCapacity=sz})=listToMaybe $ (filter (flip M.notMember m) (map Bag [1 .. sz]))


dropItem :: Inventory -> Position -> Either InventoryError (Inventory,Maybe ItemType)
dropItem i@(Inventory {invItems=m,invBagCapacity=sz}) p = do
        p2 <- checkPos sz p
        let item = M.lookup p2 m
        let m2 = M.delete p2 m
        return (i{invItems=(dropTwoHandsItem m2 item p2)},item)

dropTwoHandsItem ::(M.Map Position ItemType) -> Maybe ItemType -> Position  -> (M.Map Position ItemType)
dropTwoHandsItem m (Just (Weapon {hands=2})) RightHand= M.delete LeftHand m
dropTwoHandsItem m (Just (Weapon {hands=2})) LeftHand= M.delete RightHand m
dropTwoHandsItem m  _ _=m

checkPos :: Int -> Position -> Either InventoryError Position
checkPos capacity (Bag pos) | pos<1 = Left (InvalidBagPosition capacity)
checkPos capacity (Bag pos) | pos>capacity = Left (InvalidBagPosition capacity)
checkPos _ p = Right p

listItems :: Inventory -> [(Position,Maybe ItemType)]
listItems (Inventory {invItems=m,invBagCapacity=sz}) = map 
        (\x -> (x,M.lookup x m))
        ([RightHand, LeftHand, Body, Head] ++ (map Bag [1..sz]))
        
listCarriedItems :: Inventory -> [(Position,ItemType)]        
listCarriedItems i = map (\(x,y)->(x,fromJust y)) (filter (isJust . snd) (listItems i))
        
listCarriedItemsUniqueType :: Inventory -> [(Position,ItemType)]        
listCarriedItemsUniqueType = nubBy (\x y -> (snd x)==(snd y)) . listCarriedItems

listCarriedItemsUniqueObject :: Inventory -> [(Position,ItemType)]        
listCarriedItemsUniqueObject = (filter f) . listCarriedItems
        where 
                f (LeftHand,Weapon{hands=2})=False
                f _=True
        
        
getCarriedItem :: Inventory -> Position ->  Either InventoryError (Maybe ItemType)
getCarriedItem (Inventory {invItems=m,invBagCapacity=sz}) pos=do
        p2<- checkPos sz pos 
        return (M.lookup p2 m)
        
listActiveItems :: Inventory -> [ItemType]
listActiveItems i= (map snd (filter (isActive . fst) (listCarriedItems i)))
        
isActive :: Position -> Bool
isActive (Bag {})=False
isActive _ =True
        
listAllowedPositions :: Inventory -> ItemType -> [(Position,Maybe ItemType)]
listAllowedPositions i it=filter ((positionAllowed it) . fst) (listItems i)
        
positionAllowed :: ItemType -> Position -> Bool
-- everything can go in the bag
positionAllowed _ (Bag {})=True
positionAllowed (Weapon {}) RightHand=True
positionAllowed (Weapon {}) LeftHand=True
-- only armor on body
positionAllowed (Armor {}) Body=True
-- only helmer on head
positionAllowed (Helmet {}) Head=True
positionAllowed (Shield {}) RightHand=True
positionAllowed (Shield {}) LeftHand=True
positionAllowed _ Body=False
positionAllowed _ Head=False
positionAllowed _ RightHand=False
positionAllowed _ LeftHand=False
