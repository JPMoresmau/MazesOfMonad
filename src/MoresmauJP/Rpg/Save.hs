-- | Saving and loading characters and games
-- (c) JP Moresmau 2009
module MoresmauJP.Rpg.Save where

import Control.Monad
import Control.Monad.Trans

import Data.List
import Data.Maybe
import Data.Time

import MoresmauJP.Maze1.Maze 
import MoresmauJP.Rpg.Character
import MoresmauJP.Rpg.MazeObjects

import System.Directory
import System.FilePath
import System.IO

import Text.Printf
import System.IO.Error (catchIOError)

data RPGState = RPGState {
  rpgCharacter::(Maybe Character)
  ,mgs::(Maybe RPGGameState)
  ,fp:: FilePath}
  deriving (Show,Read,Eq)

data RPGGameState = RPGGameState {
  mazegameworld::GameWorld
  ,objects::MazeObjects
  ,tickCount::Int
  }
  deriving (Show,Read,Eq)

data BackupState = BackupState {
  bckDate::UTCTime,
  bckCharacter::Character,
  bckGame::Maybe RPGGameState
  }  
  deriving (Show,Read)

backupExtension="sav"

toBackupState :: RPGState -> IO(BackupState)
toBackupState (RPGState {rpgCharacter=Nothing})=error "toBackupState: empty game state"
toBackupState (RPGState {rpgCharacter=(Just char),mgs=mgs})=do
  t<-getCurrentTime
  return (BackupState t char mgs)  

fromBackupState :: RPGState -> BackupState -> RPGState
fromBackupState rs  bs = rs{rpgCharacter=(Just $ bckCharacter bs), mgs=(bckGame bs)}

getFile :: String -> String -> RPGState -> IO(FilePath)
getFile name ext (RPGState {fp=fp})= do
  createDirectoryIfMissing True fp
  let fileName=fp </> (makeValid $ (addExtension name ext))
  return fileName  

getGameFileForName :: RPGState -> Name -> IO(FilePath)
getGameFileForName rs name=getFile name backupExtension rs

getGameFileForCharacter :: RPGState -> Character -> IO(FilePath)
getGameFileForCharacter rs c=getGameFileForName rs (name c)
 
getBackupFileForCharacter :: RPGState -> Character -> IO(FilePath)
getBackupFileForCharacter rs c=checkBackupFile rs (name c) 1
    
checkBackupFile rs name ix = do
  f<-getFile (addExtension name (show ix)) backupExtension rs
  ex<-doesFileExist f
  if not ex 
    then return f
    else checkBackupFile rs name (ix+1)
    
saveCurrent :: RPGState -> IO (Either String (String,String))
saveCurrent (RPGState {rpgCharacter=Nothing})=return $ Right ("Nothing to save","")
saveCurrent rs@(RPGState {rpgCharacter=Just c1})=do
  if isOutOfService c1
    then do
      catchIOError (
        do
          fileNameC<-getFile (name c1) backupExtension rs
          removeFile fileNameC
          return $ Right ("Character erased","")
        )
        (\e -> return (Left ("Could not erase character: " ++ (show e))))
    else do  
      saveCurrentGame rs
      

saveCurrentGame :: RPGState -> IO (Either String (String,String))
saveCurrentGame (RPGState {rpgCharacter=Nothing})=error "saveCurrentGame: no character"
saveCurrentGame (RPGState {mgs=Nothing})=error "saveCurrentGame: no game state"
saveCurrentGame rs@(RPGState {rpgCharacter=Just c1})=do 
  fileNameG<-getGameFileForCharacter rs c1 
  saveGame rs fileNameG
  
listFiles :: RPGState -> String -> IO ([FilePath])
listFiles (RPGState {fp=fp}) ext =do  
  createDirectoryIfMissing True fp
  fps<-getDirectoryContents fp
  return $ (map (makeRelative fp)) $  filter (isSuffixOf ext) fps  
  
listGames :: RPGState -> Name -> IO ([Either String (FilePath,String)])
listGames rs name= do
  files<-listFiles rs backupExtension 
  let myFiles=filter (isPrefixOf name) files
  foldM (\list x->do
    m<-withBackup rs x (\bs->do
      if isJust $ bckGame bs 
        then return $ Just (dropExtension x,formatTime defaultTimeLocale (iso8601DateFormat $ Just "%R") $ bckDate bs) 
        else return Nothing
        )
    case m of
      Right (Just x)-> return ((Right x):list)
      Right (Nothing)-> return list
      Left a-> return ((Left a):list)
      ) [] myFiles
  
deleteCharacter :: RPGState -> String -> IO (Either String String)
deleteCharacter rs@(RPGState {fp=fp}) name=do
  files<-listFiles rs backupExtension 
  let myFiles= filter (isPrefixOf name) files
  catchIOError (do
    mapM_ (removeFile . (combine fp)) myFiles
    return $ Right (printf "Character %s deleted" name)
    )
    (\e->return $ Left (printf "The character couldn't be deleted: %s" (show e))) 

deleteGame :: RPGState -> String -> IO (Either String String)
deleteGame rs name=do
  file<-getFile name backupExtension rs 
  catchIOError (do
    if hasExtension name
      then do
        removeFile file
        return $ Right "Game deleted"
      else
        withBackupCond rs (takeFileName file) 
          (\(BackupState{bckCharacter=c})->do
            l<-saveCharacter rs c
            case l of
              Right _-> return $ Right "Game deleted"
              a->return a)
    
    )
    (\e->return $ Left (printf "The game couldn't be deleted: %s" (show e))) 


listCharacters :: RPGState -> IO ([String])
listCharacters rs= do
  files<-listFiles rs backupExtension 
  return $ filter (not . hasExtension) $ map dropExtension files  
  
saveBackup :: RPGState -> IO (Either String (String,String))
saveBackup (RPGState {rpgCharacter=Nothing}) = return $ Right ("Nothing to save","")
saveBackup rs@(RPGState {rpgCharacter=(Just c1)}) = do
  fileNameB<-getBackupFileForCharacter rs c1
  saveGame rs fileNameB
  
saveGame :: RPGState -> String -> IO (Either String  (String,String))
saveGame rs fileNameB= do  
  catchIOError (
      do
        bs<-toBackupState rs
        writeFile fileNameB (show bs)
        return (Right (printf "Game saved to %s" fileNameB,dropExtension $ takeFileName fileNameB))
      )
      (\e -> return (Left (printf "Could not save game: %s" (show e))))
      
loadBackup :: RPGState -> String -> IO (Either String (String,RPGState))
loadBackup rs name = do
    withBackupCond rs (addExtension name backupExtension)
      (\bs->do
        let game=fromBackupState rs bs
        return $ case (mgs game) of
          Just _->Right ("Game loaded",game)
          Nothing->Left "This backup contains no started game")

doesCharacterExists :: RPGState -> String -> IO (Bool)
doesCharacterExists rs name=do
  fileName<-getFile name backupExtension rs
  doesFileExist fileName 
  
  
doesCurrentGameExists :: RPGState -> String -> IO(Bool)
doesCurrentGameExists rs shortName =do
  fileName<-getFile shortName backupExtension rs
  exists<-doesFileExist fileName
  if exists
    then do
      e<-withBackup rs fileName
        (\bs->do
          let game=fromBackupState rs bs
          return (isJust (mgs game)))
      return $ case e of
        Right b->b
        Left _->False
    else return False 
  
  
saveCharacter :: RPGState -> Character -> IO (Either String String)
saveCharacter rs c=do
  fileName<-getFile (name c) backupExtension rs
  catchIOError (do
    t<-getCurrentTime
    let bs=(BackupState t c Nothing)  
    writeFile fileName (show bs)
    return (Right $ "Character saved to "++fileName)
    )
    (\e -> return (Left $ printf "Could not save character: %s" (show e)))  
    
readF :: (MonadIO m) => RPGState ->  String -> (String -> IO (Either String a)) -> (a -> m b) -> m (Either String b)
readF (RPGState {fp=fp}) shortName act act2 = do
  let sn'=if (takeExtension shortName)==("."++backupExtension)
            then shortName
            else addExtension shortName backupExtension
  let fileName=fp </> sn'
  exists<-liftIO $ doesFileExist fileName
  if exists
    then do 
      a <- liftIO $ catchIOError (do
        a<- withFile fileName ReadMode (\h -> do
            s<-hGetContents h
            (act s)
          )
        return a)
        (\err-> return (Left (printf "The file %s could not be read: %s" fileName (show err))))
      case a of
        Right b -> do
          d<-act2 b
          return $ Right d
        Left c -> return $ Left c
    else return $ Left (printf "File %s does not exist" fileName)    

withBackup rs name m = do
  readF rs name readBackup
    m

withCharacter rs name m = do
  readF rs name readBackup
    (\bs-> m $ bckCharacter bs)

withBackupCond rs name m = do
  ei<-readF rs name readBackup
    m
  return $ case ei of
    Right (Left a)->Left a
    Right (Right a)->Right a
    Left a -> Left a    


readBackup s=do 
  catchIOError (do
    c<-(readIO s)::IO BackupState
    return $ Right c
    )
    (\err-> return (Left (printf "The backup file could not be parsed: %s" (show err))))



  
