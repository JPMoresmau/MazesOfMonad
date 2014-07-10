-- | Console based navigation in a maze
-- (c) JP Moresmau 2009
module MoresmauJP.Maze1.MazeGame where

import MoresmauJP.Util.Random
import Control.Monad.State
import Control.Monad.Writer
import Data.Char
import Data.List
import qualified Data.Set as DataSet

import MoresmauJP.Core.Screen
import MoresmauJP.Maze1.Maze

import System.Random

start :: IO()
start = do
	sg <- getStdGen
	gs <- evalRandT initialGameState (ProductionRandom sg)
	sg<-getStdGen
	((WList ls,_),_)<-runStateT (evalRandT (runWriterT $ mapAction [""]) (ProductionRandom sg)) gs
	MoresmauJP.Core.Screen.start (WList ("Maze":ls),gs)
	return ()

initialGameState ::(MonadRandom m)=>m (GameState GameWorld)
initialGameState = do
	gw<-generateGameWorld (10,5)
	return (buildGameState gw)

buildGameState :: GameWorld -> GameState GameWorld
buildGameState gw=GameState gw (getScreen gw (getDirections gw))

getScreen :: GameWorld -> [Direction] -> Maybe (Screen GameWorld)
getScreen _ dirs = 
	let
		f dir = Action (map toLower (show dir)) ("Move " ++ (show dir)) (move dir)
		actions=map f dirs
		mapA= Action "map" "See the map" mapAction
	in	
		Just (Screen (mapA:actions))
		
		
move :: Direction -> ActionFunction GameWorld
move dir _  = do
	gw <- gets gsData
	let mgw2=moveInMaze gw (getNextCell (position gw) dir)
	case mgw2 of 
			Nothing->return (WText "Ouch!")
			Just (gw2,win)->if win 
				then
					do
					put (GameState gw2 Nothing)
					return (WList ["You emerge victorious from the maze! "])
				else
					do
						let dirs=getDirections gw2
						let gs2=GameState gw2 (getScreen gw2 dirs)
						put gs2
						WList ls<-mapAction [""]
						return (WList (("You move " ++ (show dir)) : ls))
		
getDirectionsText :: [Direction] -> String
getDirectionsText dirs= "You can move to: " ++ (concat $ intersperse "," (map show dirs))
		
data Direction = North | South | East | West 
	deriving (Show, Read)

getNextCell :: Cell -> Direction -> Cell
getNextCell (x,y) North=(x,y-1)
getNextCell (x,y) South=(x,y+1)
getNextCell (x,y) West=(x-1,y)
getNextCell (x,y) East=(x+1,y)

getDirections :: GameWorld -> [Direction]
getDirections (GameWorld mz (x,y) _) = 
	let
		n=filter (\(x1,y1)-> isMovePossible (x,y) (x1,y1) mz ) (getNeighbours (x,y) mz)
		f (x1,y1) 
			| x1<x  = West
			| x1>x	= East
			| y1>y  = South
			| y1<y  = North
			| otherwise = error "getDirections: impossible coordinates"
	in map f n

mapAction :: ActionFunction GameWorld
mapAction _ =
	do
		(GameState {gsData=gw}) <- get
		let (width,height)=size $ maze gw
		let xs=[0..((width*2))]
		let mapLines=map (\y-> map (getMapCharacter gw y) xs) [0..((height*2))]
		let dirs=getDirections gw
	--let cells=[(x,y)|x<-[0..((width*2)+1)], y<-[0..((width*2)+1)]]
		return (WList (mapLines ++ [getDirectionsText dirs]))
	
getMapCharacter :: GameWorld -> Int -> Int -> Char
getMapCharacter _ _ 0 = '#'
getMapCharacter _ 0 _ = '#'
getMapCharacter gw y x 	| x == (fst (size $ maze $ gw)) * 2 = wall
						| y == (snd (size $ maze $ gw)) * 2 = wall
						| (even x) && (even y) = wall
						| (x,y) == (doubleSize $ position gw)  = '@'
						| (odd x) && (odd y) = if DataSet.member (halfSize (x,y))(explored gw)
							then 
								if (x,y) == (doubleSize $ end $ maze $ gw)
									then '$'
									else ' '
							else wall
						| even x = getMaybePassageCharacter gw (halfSize (x-1,y)) (halfSize (x+1,y)) 
						| even y = getMaybePassageCharacter gw (halfSize (x,y-1)) (halfSize (x,y+1))
						| otherwise = '?'

getMaybePassageCharacter :: GameWorld -> Cell -> Cell -> Char
getMaybePassageCharacter gw from to 
	| isMovePossible from to (maze gw) = getPassageCharacter gw from to
	| otherwise = wall 

getPassageCharacter :: GameWorld -> Cell -> Cell -> Char
getPassageCharacter gw from to 
	| DataSet.member from (explored gw) && DataSet.member to (explored gw) = ' '
	| otherwise = wall


wall :: Char
wall = '#'

doubleSize :: Cell -> Cell
doubleSize (x,y)=(x*2-1,y*2-1)

halfSize :: Cell -> Cell
halfSize (x,y)=(div (x+1) 2 ,div (y+1) 2)