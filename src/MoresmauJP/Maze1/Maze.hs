-- | Maze handling code: generate and move in the maze
-- (c) JP Moresmau 2009
module MoresmauJP.Maze1.Maze 
where

import MoresmauJP.Util.Lists
import MoresmauJP.Util.Random

import Data.List
import qualified Data.Map as DataMap
import qualified Data.Set as DataSet
import Data.Maybe



data GameWorld = GameWorld {
	maze::Maze
	,position::Cell,
	explored::CellSet}
	deriving (Show, Read,Eq)

type Cell=(Int,Int)
type Size=(Int,Int)
type Edges=[Cell]

type CellMap = DataMap.Map Cell Edges
type CellSet = DataSet.Set Cell

data Maze=Maze {cellmap::CellMap
	,start::Cell
	,end::Cell
	,size::Size
	}
	deriving (Read,Show,Eq)

isWinning :: GameWorld -> Bool
isWinning gw= (position gw) == (end $ maze gw)

moveInMaze :: GameWorld -> Cell -> Maybe (GameWorld,Bool)
moveInMaze gw toCell | 
	isMovePossible (position gw) toCell (maze gw) =
		let 
			expls=explored gw
			expls2=DataSet.union expls (DataSet.fromList (getNeighbours toCell (maze gw)))
			--nub (expls ++ (getNeighbours toCell (maze gw)))
			gw2=gw{position=toCell,explored=expls2}
			win=isWinning gw2
		in Just (gw2,win)
	| otherwise = Nothing	

randomMove :: (MonadRandom m) => GameWorld -> m GameWorld
randomMove gw=do
	let expls=explored gw
	toCell <- randomPickp (DataSet.elems (DataSet.delete (position gw) expls))
	let expls2=DataSet.union expls (DataSet.fromList (getNeighbours toCell (maze gw)))
	return (gw{position=toCell,explored=expls2})

isMovePossible :: Cell -> Cell -> Maze -> Bool
isMovePossible from to mz =  elem to (getNeighbours from mz)

getNeighbours :: Cell -> Maze -> Edges
getNeighbours from mz =
	fromJust $ DataMap.lookup from (cellmap mz)
	
	
generateGameWorld:: (MonadRandom m) =>Size -> m GameWorld
generateGameWorld sz= do
	maze <- generateMaze sz
	let startCell=start maze
	let explored= DataSet.insert startCell (DataSet.fromList (getNeighbours startCell maze))
  	return GameWorld {maze=maze,position=startCell,explored=explored}

generateMaze :: (MonadRandom m) =>Size -> m Maze
generateMaze sz = do
	firstX<-getRandomRange (1,fst sz)
	firstY<-getRandomRange (1,snd sz)
	--(firstX,firstY)
	let m1=DataMap.fromList [((firstX,firstY),[(firstX,firstY)])]
	let frontiers=getUnprocessedNeighbours (firstX,firstY) sz m1
	let m2=DataMap.union m1 (DataMap.fromList(map (\x->(x,[])) frontiers))
	randomFrontiers<-randomHeadp frontiers
	(m3,end) <- mazeStep m2 sz randomFrontiers
	let m4=removeFirstCell m3 (firstX,firstY) 
	return (Maze {cellmap=m4,start=(firstX,firstY),end=end,size=sz})

removeFirstCell :: CellMap -> Cell -> CellMap
removeFirstCell cm c= 
	let
		Just l = DataMap.lookup c cm
	in
		 DataMap.insert c (init l) cm

mazeStep :: (MonadRandom m) =>CellMap -> Size -> Edges -> m (CellMap,Cell)
mazeStep _ _ []=error "mazeStep: empty edges"
mazeStep cm sz (f:fs) = do
	let nbs=getInNeighbours f sz cm
	nb <- randomPickp nbs
	let edges=DataMap.lookup nb cm
	let cm1=
		case edges of
			Just l 	-> DataMap.insert nb (f:l) cm
			Nothing -> DataMap.insert nb [f] cm
	let cm2 = DataMap.insert f [nb] cm1
	let frontiers=getUnprocessedNeighbours f sz cm2
	let cm3= DataMap.union cm2 (DataMap.fromList(map (\x->(x,[])) frontiers))
	randomFrontiers <- randomHeadp (fs ++ frontiers)
	if null randomFrontiers
		then
			return (cm3,f)
		else
			mazeStep cm3 sz randomFrontiers
	
getLeftNeighbour :: Cell ->  Edges
getLeftNeighbour (1,_) =[]
getLeftNeighbour (x,y) =[(x-1,y)]

getRightNeighbour :: Cell -> Size -> Edges
getRightNeighbour (x,y) (width,_)
	| (x==width)   =[]
	| otherwise   =[(x+1,y)]

getAllNeighbours :: Cell -> Size -> Edges
getAllNeighbours c sz =
	(getLeftNeighbour c) ++ (getRightNeighbour c sz)
	++ (map swap (getLeftNeighbour (swap c)))
	++ (map swap (getRightNeighbour (swap c) (swap sz)))

getUnprocessedNeighbours :: Cell -> Size -> CellMap -> Edges
getUnprocessedNeighbours c sz cm = filter (flip DataMap.notMember cm)(getAllNeighbours c sz)

getInNeighbours :: Cell -> Size -> CellMap -> Edges
getInNeighbours c sz cm = filter (isIn cm) (getAllNeighbours c sz)
	where isIn lm key = 
		let val=DataMap.lookup key lm
		in
			case val of
				Just l	->not (null l)
				Nothing	->False

concatRev:: [a] -> [a] -> [a]
concatRev [] l2=l2
concatRev (x:xs) l2=concatRev xs (x:l2)

