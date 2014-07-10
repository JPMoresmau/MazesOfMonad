-- | Console handling code
-- (c) JP Moresmau 2009
module MoresmauJP.Core.Screen where


import Control.Monad.State
import Control.Monad.Writer
import Data.Char
import Data.List
import Data.Maybe

import MoresmauJP.Util.Random

-- | a Screen represent a certain UI state, with possible actions
data Screen a = Screen {
        actions::[Action a] -- ^ possible action in that screen
        }

-- | Action doable in the ui
data Action a = Action {
        actionName::String -- ^ the name of the action (what to type)
        , actionDescription::String -- ^ the description (showed when using help)
        , actionFunction::(ActionFunction a) -- ^ the function to call
        }

-- | a Monad transformer, wrapping state (a), IO and Random
type ScreenT a b= (RandT (StateT a IO)) b 

-- | Screen Message to display
type ScreenMessage = String

-- | Screen Messages
type ScreenMessages = [ScreenMessage]

-- | add a ScreenMessage to the current messages
addScreenMessage :: (MonadWriter ScreenMessages m,Monad m)=> ScreenMessage -> m ()
addScreenMessage msg=tell [msg]
        
-- | A Monad Transformer, ScreenT with GameState and returning Widgets
type GSWScreenT a= ScreenT (GameState a) (Widget a)

-- | The real Monad Transformer used: allows to write messages, get random number, stores
-- a state of a GameState containing a, returning Widgets
type WScreenT a=WriterT ScreenMessages (RandT (StateT (GameState a) IO)) (Widget a)

instance (Monad m) =>  MonadRandom (WriterT ScreenMessages (RandT m)) where
        getRandomRange =lift . getRandomRange
        getSplit = lift getSplit


-- | An Action function get its arguments typed in the GUI and works in a WScreenT
type ActionFunction a = [String] -> WScreenT a

-- | GameState: the data (a) and the current screen
data GameState a = GameState {gsData::a -- ^ the data
        , screen::Maybe (Screen a) -- ^ the current screen
        }

-- | Widgets are the actual ui component returned by an action
data Widget a= 
        -- | simple line of text
        WText {
                wtext::String -- ^ line of text
                }
        -- | list of items
        | WList {witems :: [String] -- ^ lines of text
                } 
        -- | ask for the user to type in a string
        | WInput {
                witems :: [String] -- ^ lines of text
                , wact::(String -> WScreenT a) -- ^ handler action
                }
        -- | choose an item from a list
        | WCombo {
                witems :: [String] -- ^ lines of text
                ,wlist :: [String] -- ^ list items
                ,wact :: (String -> WScreenT a) -- ^ handler action
                }
        -- | asks a yes/no question 
        | WCheck {
                witems :: [String]  -- ^ lines of text
                ,wquestion :: String -- ^ question 
                ,wdefault :: Bool -- ^ default answer 
                ,wbact :: (Bool -> WScreenT a) -- ^ handler action
                }
        -- | placeholder for no action 
        | WNothing

-- | a widget and a gamestate
type ScreenState a = (Widget a,GameState a)

-- | get a Combo widget that uses the show method of the objects passed to automatically build the list
getShowCombo :: Show b => [String] -- ^ lines of text
        -> [b] -- ^ objects to display in list 
        ->  ((ComboResult b) -> WScreenT a) -- ^ handler action (gets the selected object as parameter
        -> Widget a -- ^ the resulting widget
getShowCombo= getMappedCombo show

-- | get a Combo widget that uses an arbitrary method of the objects passed to automatically build the list
getMappedCombo :: (b-> String) -- ^ the method to use to translate the object in a string from the menu
        -> [String] -- ^ lines of text
        -> [b] -- ^ objects to display in list 
        ->  ((ComboResult b) -> WScreenT a) -- ^ handler action (gets the selected object as parameter
        -> Widget a -- ^ the resulting widget
getMappedCombo myShow s objs af=
        let 
                objWithNames=map (\x-> (x,myShow x)) objs
                af2=(\s2 -> do 
                        if null s2
                                then
                                        af Empty
                                else
                                        do
                                        let objChosen=listToMaybe (map fst (filter (\x-> (snd x) == s2) objWithNames))
                                        case objChosen of
                                                Just oc-> af (Exact oc)
                                                Nothing-> af (Unknown s2)
                                                )
        in (WCombo s (map snd objWithNames) af2)
                
-- | checks if the rest of the command line is already an option from the menu
getPretypedWidget :: Widget a -- ^ the original widget
        -> [String] -- ^ the rest of the parameters typed by the user
        -> WScreenT a -- ^ the result
getPretypedWidget wc@(WCombo _ choices af) (typed:_) = do
        -- see if the next parameter is a number corresponding to one of the items in the list
        let chosen=filter (\(x,_)-> x==typed) (zipWith (\a b -> ((show a),b)) [1..] choices)
        if (null chosen)
                then return wc
                -- we have a match, run the action
                else af (snd $ head chosen)
getPretypedWidget w _=return w
                
removeWithName :: [Action a] -> [Action b] -> [Action a]
removeWithName aa ab=
        let names=(map actionName aa) \\ (map actionName ab)
        in filter (\a -> elem (actionName a) names) aa

-- | result from a user action
data ComboResult a= Empty -- ^ no result, nothing chosen
         | Unknown String -- ^ unknown result (typed something not in a list)
         | Exact a -- ^ proper choice
        deriving (Show,Read)
        
-- | start a UI loop
start :: ScreenState a -- ^ initial state
        -> IO(a) -- ^ result
start (w,gs)=do
        GameState s2 _ <- ioRandT (commandLoop2 w) gs
        return s2
        
-- | internal UI loop
commandLoop2 :: Widget a -- ^ current widget to render 
        -> GSWScreenT a -- ^ screen monad we run in
commandLoop2 w = do
        af<- liftIO $ renderWidget w
        scr <- gets screen
        if (isJust scr)
                then
                        if isJust af
                                then
                                        do
                                                (w2,msgs) <- runWriterT $ fromJust af
                                                when (not $ null msgs) (liftIO $ (mapM_ putStrLn msgs))        
                                                commandLoop2 w2
                                else                
                                        do
                                                liftIO $ putStr ">"
                                                input <- liftIO $ getLine
                                                let cmds = words input
                                                if null cmds
                                                        then
                                                                commandLoop2 WNothing
                                                        else
                                                                do
                                                                        let (cmd:_)=cmds
                                                                        let af2 = getAction (map Data.Char.toLower cmd) (actions $ fromJust scr)
                                                                        (w2,msgs)<- runWriterT (af2 cmds)
                                                                        when (not $ null msgs) (liftIO $ (mapM_ putStrLn msgs))        
                                                                        commandLoop2 w2
                else
                        return WNothing

-- | render a widget onto the console
renderWidget :: Widget a -- ^ the widget to render        
        -> IO(Maybe(WScreenT a)) -- ^ the result (may be empty)
renderWidget (WNothing)= do
        return Nothing
renderWidget (WText s)= do
        putStrLn s
        return Nothing
renderWidget (WList ss)=do
        mapM_ putStrLn ss
        return Nothing
renderWidget (WInput ss1 af)=do
        mapM_ putStrLn ss1
        input <- getLine
        return (Just $ af input)
renderWidget (WCheck ss1 s def af)=do
        mapM_ putStrLn ss1
        let choices=if def then " (Y/n)" else " (y/N)"
        putStrLn (s ++choices)
        cmds <- getArgs
        let ch=if null cmds
                then def
                else (map toUpper (head cmds))=="Y"
        return (Just (af ch))
renderWidget (WCombo ss1 ss2 af)=do
        mapM_ putStrLn ss1
        let choices=zipWith (\a b -> ((show a),b)) [1..] ss2
        mapM_ putStrLn (map (\(a,b) -> a ++ ": "++b) choices)
        cmds <- getArgs
        let chosen=if null cmds
                then [("","")]
                else filter (\(x,_)-> x==(head cmds)) choices
        if null chosen
                then return (Just $ af "")
                else return (Just $ af (snd $ head chosen))
                        
-- | get arguments typed in at the command line        
getArgs :: IO([String])        -- ^ the resulting arguments                        
getArgs = do
        input <- getLine
        return (words input)

-- | show help with available commands                        
help :: Bool -- ^ display help on system commands too? 
        -> ActionFunction a -- ^ the handler function 
help withSystem _ = do
        let        f (Action s1 s2 _)= (s1++": "++s2)
        let        sysLines= if withSystem
                        then (map f systemActions)
                        else []
        gs <- get
        let acts=actions $ fromJust $ screen gs
        let wl=WList (sort ( sysLines ++
                        (map f acts)))
        return (wl) 

-- | default handler for unknown actions
unknown :: ActionFunction a
unknown args = return (WText ("I do not understand the command " ++ (head args)))

-- | quit the game action
quit :: ActionFunction a
quit _ = do
        modify (\gs-> gs{screen=Nothing})
        return (WText ("Bye bye, hope you enjoyed the game!")) 

-- | when several actions could fit what the user typed, display the list of full names
choice :: [String] -- ^ possible actions
        -> ActionFunction a -- ^ resulting handler
choice ss _ = return (WList ss)

-- | back action
backAction :: Screen a -- ^ Screen to go back to 
        -> Action a -- ^ resulting action
backAction sc=Action "back" "Go back to main screen" (back sc)

-- | back handler
back :: Screen a -- ^ Screen to go back to 
        -> ActionFunction a -- ^ result
back sc _ =do
        (GameState a _) <- get
        put (GameState a (Just sc))
        return (WText "Back")

-- | default system actions
systemActions :: [Action a]
systemActions = [Action "help" "Provides help on available actions" (help True)
        ,Action "?" "Provides help on available actions" (help True)
        ,Action "quit" "Exit the game" quit]
                                
-- | get action from what the user typed and the possible actions
getAction :: String -- ^ the first word the user typed 
        -> [Action a] -- ^ the possible actions 
        -> ActionFunction a -- ^ the chosen action handler
getAction "help" _ = help True
getAction cmd acts =
        let        
                filt=(filter (\x-> isPrefixOf cmd (map Data.Char.toLower (actionName x))))
                possible=(filt systemActions) ++ (filt acts)
                l = length possible
        in
                if l==0 then
                        unknown
                else if l==1 then
                        actionFunction $ head possible
                else 
                        choice (map (\(Action s1 _ _)-> s1) possible)

-- | combine a widget with maybe another
combineMaybeWidget :: Widget a -- ^ initial widget 
        -> Maybe (Widget a) -- ^ next widget 
        -> Widget a -- ^ resulting widget
combineMaybeWidget w Nothing = w
combineMaybeWidget w1 (Just w2) =combineWidget w1 w2
        
-- | combine a widget with another        
combineWidget :: Widget a -- ^ initial widget 
        -> Widget a -- ^ next widget 
        -> Widget a -- ^ resulting widget
combineWidget WNothing a=a
combineWidget a WNothing=a
combineWidget (WText s1) (WText s2)=WList [s1,s2]
combineWidget (WText s1) (WList ss2)=WList (s1:ss2)
combineWidget (WText s1) (WInput ss1 ss2)=WInput (s1:ss1) ss2
combineWidget (WText s1) (WCheck ss1 ss2 ss3 ss4)=WCheck (s1:ss1) ss2 ss3 ss4
combineWidget (WText s1) (WCombo ss1 ss2 af)=WCombo (s1:ss1) ss2 af
combineWidget (WList ss1) (WText s2)=WList (ss1++[s2])
combineWidget (WList ss1) (WList ss2)=WList (ss1++ss2)
combineWidget (WList ss1) (WInput ss2 ss3)=WInput (ss1++ss2) ss3
combineWidget (WList s1) (WCheck ss1 ss2 ss3 ss4)=WCheck (s1++ss1) ss2 ss3 ss4
combineWidget (WList s1) (WCombo ss1 ss2 af)=WCombo (s1++ss1) ss2 af
combineWidget _ _=error "combineWidget: undefined combination"
