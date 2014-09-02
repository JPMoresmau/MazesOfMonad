-- | Random handling utilities, including a monad for keeping random generators
-- (c) JP Moresmau 2009
module MoresmauJP.Util.Random where

import Control.Monad.Identity
import Control.Monad.State
import System.Random
import Control.Applicative (Applicative)

class (Monad m) => MonadRandom m where
  getRandomRange :: (Int,Int) -> m Int
  getSplit :: m RandomWrapper

newtype RandT m a = RandT (StateT RandomWrapper m a)
    deriving (Functor, Monad, MonadTrans, MonadIO, Applicative)

liftState :: (MonadState s m) => (s -> (a,s)) -> m a
liftState t = do v <- get
                 let (x, v') = t v
                 put v'
                 return x
 
instance (Monad m) => MonadRandom (RandT m) where
  getRandomRange (low,high) = RandT . liftState $ randomRange (low,high)
  getSplit = RandT . liftState $ splitWrapper

evalRandT :: (Monad m) => RandT m a -> RandomWrapper -> m a
evalRandT (RandT x) g = evalStateT x g
 
runRandT  :: (Monad m) => RandT m a -> RandomWrapper -> m (a, RandomWrapper)
runRandT (RandT x) g = runStateT x g

--identRandT :: (Monad m) => RandT m a -> RandomWrapper -> Identity a
identRandT r g= runIdentity (evalRandT r g)

ioRandT r s=do
  sg<-getStdGen
  execStateT (evalRandT r (ProductionRandom sg)) s

instance (MonadState s m) => MonadState s (RandT m) where
    get = lift get
    put = lift . put


newtype Rand a = Rand (RandT Identity a)
    deriving (Functor, Monad, MonadRandom, Applicative)
 
evalRand :: Rand a -> RandomWrapper -> a
evalRand (Rand x) g = runIdentity (evalRandT x g)

runRand :: Rand a -> RandomWrapper -> (a, RandomWrapper)
runRand (Rand x) g = runIdentity (runRandT x g)
 
evalRandIO :: Rand a -> IO a
evalRandIO (Rand (RandT x)) =do
  sg<-getStdGen
  return $ fst $ (runIdentity . runStateT x) (ProductionRandom sg)

data RandomWrapper =TestRandom [Int]
  | ProductionRandom StdGen
  deriving (Show,Read)
  
mkTestWrapper l=TestRandom (cycle l)  
  
randomRange :: (Int,Int) -> RandomWrapper -> (Int,RandomWrapper)
randomRange _ (TestRandom [])=error "empty TestRandom"
randomRange _ (TestRandom (i:l))=(i,TestRandom l)
randomRange bounds (ProductionRandom g)=let
  (i,g')=randomR bounds g
   in (i,ProductionRandom g')
   
   
randomRanges:: (Int,Int) -> RandomWrapper -> Int  -> ([Int],RandomWrapper)
randomRanges rang gen nb=    
  foldl (\(r,gen) rang -> 
    let (r',gen')= randomRange rang gen 
    in ((r++[r']),gen') 
    ) ([],gen) (replicate nb rang)
    
splitWrapper :: RandomWrapper -> (RandomWrapper,RandomWrapper)
splitWrapper t@(TestRandom {})=(t,t)
splitWrapper (ProductionRandom g)=let (g1,g2)=split g
  in (ProductionRandom g1,ProductionRandom g2)