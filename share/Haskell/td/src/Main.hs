{-# LANGUAGE Arrows #-}

module Main where
import Options.Applicative
import Text.XML.HXT.Core
import Text.XML.HXT.RelaxNG
import Text.XML.HXT.Arrow.Pickle
import Data.Text (Text, pack, unpack, split)
import Data.List
import Control.Monad
import System.Console.ANSI
import qualified Data.Tree.Class as DTC
import qualified Text.XML.HXT.DOM.XmlNode as HXTDOM
import Data.Map
import Data.Maybe
import System.Directory
import System.Process
import System.IO
import Data.Time


tdhsXML = ".config/amkhlv/td.hs.xml"
tdhsRNG = ".config/amkhlv/td.hs.rng"

data Clops = Clops
             { listName :: Maybe String
             , n :: Maybe Int
             , add :: Maybe String
             , del :: Bool
             , full :: Bool
             , ff :: Bool
             , dir :: Bool
             , urg :: Bool
             , showAll :: Bool
             }

cloparser :: Parser Clops
cloparser = Clops
  <$> optional ( argument str (metavar "LISTNAME"))
  <*> optional ( option auto ( short 'n' <> metavar "N" <> help "Select entry"))
  <*> optional ( option str ( short 'a' <> metavar "A" <> help "Add"))
  <*> switch ( short 'd' <> help "Delete" )
  <*> switch ( short 'f' <> help "Whether to give full description" )
  <*> switch ( long "ff" <> help "Open URL in Firefox")
  <*> switch ( long "dir" <> help "print path to the project directory")
  <*> switch ( long "urg" <> help "urgent flag")
  <*> switch ( long "all" <> help "show all items included postponed")

data Cfg = Cfg
  { rngFile :: String
  , todoList :: TodoListMap
  } deriving (Show, Eq)

instance XmlPickler Cfg where xpickle = xpCfg
xpCfg :: PU Cfg
xpCfg =
  xpElem "config" $
  xpWrap ( \((r,t)) -> Cfg r t , \cf -> (rngFile cf, todoList cf) ) $
  xpPair (xpElem "rngFile" xpText) (xpElem "todolists" xpTodoListMap)

type TodoListMap = Map String PathTs
xpTodoListMap :: PU TodoListMap
xpTodoListMap =
  xpWrap ( fromList, toList ) $
  xpList $
  xpElem "todolist" $
  xpPair (xpAttr "name" xpText) xpPathTs

type PathTs = (String, Maybe String)
xpPathTs :: PU PathTs
xpPathTs =
  xpWrap ( \((t,p)) -> (p,t) , \x -> (snd x, fst x) ) $
  xpPair (xpOption (xpElem "ts" xpText)) (xpElem "path" xpText)

data TodoItem = TodoItem
  { nick :: String
  , deadline :: Maybe String
  , description :: Maybe String
  , url :: Maybe String
  , directory :: Maybe String
  , postponed :: Maybe String
  , urgent :: Maybe String
  } deriving (Show, Eq)
instance XmlPickler TodoItem where
  xpickle = xpTodoItem
xpTodoItem :: PU TodoItem
xpTodoItem =
  xpElem "todo" $
  xpWrap ( \ ((ni, dl, desc, url, dir, pp, urgent)) -> TodoItem ni dl desc url dir pp urgent
         , \t -> (nick t, deadline t, description t, url t, directory t, postponed t, urgent t)
         ) $
  xp7Tuple
  (xpAttr "nick" xpText)
  (xpOption (xpElem "deadline" xpText))
  (xpOption (xpElem "description" xpText))
  (xpOption (xpElem "url" xpText))
  (xpOption (xpElem "directory" xpText))
  (xpOption (xpElem "after" xpText))
  (xpOption (xpElem "urgent" xpText0))
  
toDoToXml :: TodoItem -> XmlTree 
toDoToXml tdi = head $ DTC.getChildren (pickleDoc xpTodoItem tdi)
-- because an HXT parse tree always starts with a root element (tag) of name "/"

showItem :: Int -> (Maybe TodoItem) -> IO ()
showItem n (Just (TodoItem nick mdl mdesc murl mdir mpp murgent)) = do
  setSGR [SetColor Foreground Vivid Yellow, SetConsoleIntensity BoldIntensity]
  putStr $ (if (n < 10) then (" " ++ (show n)) else show n)
  setSGR [Reset]
  case murgent of
    Just _ -> setSGR [SetColor Foreground Vivid Red, SetBlinkSpeed SlowBlink] >> putStr " ■ "
    Nothing -> setSGR [SetColor Foreground Vivid Green] >> putStr " ▬ "
  setSGR [Reset]
  putStrLn (case mdl of
              Just x -> nick ++ " [" ++ x ++ "]"
              Nothing -> nick)
showItem _ Nothing = putStrLn "ERROR"

readYMD :: String -> String -> String -> (Integer, Int, Int)
readYMD yyyy mm dd = (read yyyy, read mm, read dd)

timeHasCome :: TodoItem -> (UTCTime, TimeZone) -> Bool
timeHasCome tdi (ct, ctz) =
  case (postponed tdi) of
    Just p ->
      let [yyyy, mm, dd] = Data.List.map unpack $ Data.Text.split (== '-') (pack p) in
      let (yN, mN, dN) = readYMD yyyy mm dd in
      case (fromGregorianValid yN mN dN) of
        Just dpp -> (diffDays dpp (localDay $ utcToLocalTime ctz ct)) <= 0 
    Nothing -> True

ns :: (UTCTime, TimeZone) -> [Maybe TodoItem] -> [Int]
ns ttz tdis = [ (fst p) | p <- (zip [0..] $ fromJust <$> tdis), timeHasCome (snd p) ttz]

readTodoListXML r x = readDocument [withRelaxNG r, withRemoveWS yes] x
getTodoItems :: String -> String -> IO [Maybe TodoItem]
getTodoItems r x = runX $ readTodoListXML r x  >>> getChildren >>> getChildren >>> isElem >>> (processChildren isElem) >>^ (unpickleDoc xpTodoItem)

addItem :: String -> String -> String -> Bool -> IO ()
addItem r x a urg = do
  let murg = if urg then Just "" else Nothing
  runX $
      readTodoListXML r x  >>>
      (processChildren (processChildren isElem)) >>>
      (processChildren (changeChildren (\xmls -> (toDoToXml (TodoItem a Nothing Nothing Nothing Nothing Nothing murg)):xmls))) >>>
      writeDocument [withIndent yes] x
  return ()

gtdShow :: String -> String -> Bool -> (UTCTime, TimeZone) -> IO ()
gtdShow r x sa ttz = do
  items <- getTodoItems r x
  if sa then
    sequence_ [ showItem n item | (n, item) <- zip [1..] items]
    else
    putStrLn (show $ ns ttz items) >>
    sequence_ [ showItem n (items !! nreal) | (n, nreal) <- zip [1..] (ns ttz items) ]

gtdDelete :: String -> String -> Int -> Bool -> (UTCTime, TimeZone) -> IO ()
gtdDelete r x n0 sa ttz =
  do
    items <- getTodoItems r x
    let n = if sa then n0 else (ns ttz items) !! n0
    putStrLn "delete this?"
    c <- getChar
    case c of
      'y' -> runX $
        readTodoListXML r x  >>>
        (processChildren (processChildren isElem)) >>>
        (processChildren (changeChildren (\xmls -> (take (n - 1) xmls) ++ (drop n xmls)))) >>>
        writeDocument [withIndent yes] x
      _ -> return []
    return ()

gtdFull :: String -> String -> Int -> Bool -> (UTCTime, TimeZone) -> IO ()
gtdFull r x n0 sa ttz = do
  items <- getTodoItems r x
  let n = if sa then n0 - 1 else (ns ttz items) !! (n0 - 1)
  case (items !! n) of -- TodoItem nick deadline description url dir
    Just (TodoItem a mdl mdsc murl mdir mpp murgent) ->
      setSGR [SetColor Foreground Vivid Green] >> 
      putStr a >> setSGR [Reset] >>
      do
        case mdl of
          (Just dl) -> setSGR [SetColor Foreground Vivid Red] >> putStrLn (" [" ++ dl ++ "]") >> setSGR [Reset]
          _ -> putStrLn ""
        case mdsc of
          (Just dsc) -> putStrLn dsc
          _ -> return ()
        case murl of
          (Just url) -> setSGR [SetColor Foreground Vivid Blue, SetUnderlining SingleUnderline] >> putStrLn url >> setSGR [Reset]
          _ -> return ()
        case mdir of
          (Just dir) -> setSGR [SetColor Foreground Vivid Yellow] >> putStrLn dir >> setSGR [Reset]
          _ -> return ()
        case mpp of
          (Just pp) -> setSGR [SetColor Foreground Vivid Green] >> putStr "postpone until " >> putStrLn pp >> setSGR [Reset]
          _ -> return ()
    _ -> putStrLn "ERROR"

gtdOpenInBrowser :: String -> String -> Int -> Bool -> (UTCTime, TimeZone) -> IO ()
gtdOpenInBrowser r x n0 sa ttz = do
  items <- getTodoItems r x
  let n = if sa then n0 - 1 else (ns ttz items) !! (n0 - 1)
  case (items !! n) of
    Just (TodoItem a _ _ (Just url) _ _ _) -> spawnCommand ("firefox '" ++ url ++ "'") >> putStrLn "opening in Firefox"
    _ -> putStrLn (show items) >> putStrLn "-- no URL to open"

gtdCD :: String -> String -> Int -> Bool -> (UTCTime, TimeZone) -> IO ()
gtdCD r x n0 sa ttz = do
  home <- getHomeDirectory
  current <- getCurrentDirectory
  items <- getTodoItems r x
  let n = if sa then n0 - 1 else (ns ttz items) !! (n0 - 1)
  case (items !! n) of
    Just (TodoItem a _ _ _ (Just dir) _ _) -> putStr (home ++ "/" ++ dir)
    _ -> putStr current

gtdShowLists :: TodoListMap -> IO ()
gtdShowLists tdlm = (sequence_ $ [ putStr k | k <- (intersperse " " (keys tdlm))]) >> putStrLn ""

getConf :: Clops -> IO (Clops, Maybe Cfg)
getConf cl =
  do
    home <- getHomeDirectory
    mc <- runX $ readDocument [ withRelaxNG (home ++ "/" ++ tdhsRNG)
                              , withRemoveWS yes
                              ] (home ++ "/" ++ tdhsXML) >>>
      getChildren >>> isElem >>> (processChildren isElem) >>^ (unpickleDoc xpCfg)
    return (cl, head mc)

getTTZ :: (Clops, Maybe Cfg) -> IO (Clops, Maybe Cfg, (UTCTime, TimeZone))
getTTZ (cl, mcfg) = do
  ttz <- (,) <$> getCurrentTime <*> getCurrentTimeZone
  return (cl, mcfg, ttz)

checkTS :: String -> Cfg -> (UTCTime, TimeZone) -> IO Bool
checkTS lName cfg ttz = do
  let expectedTS = formatTime defaultTimeLocale (iso8601DateFormat Nothing) (utcToLocalTime (snd ttz) (fst ttz))
  return $ case (snd ((todoList cfg) ! lName)) of
             Just ts -> (expectedTS == ts)
             Nothing -> False

insertTS :: String -> Cfg -> (UTCTime, TimeZone) -> IO ()
insertTS lName cfg ttz = do
  home <- getHomeDirectory
  let  newTS = formatTime defaultTimeLocale (iso8601DateFormat Nothing) (utcToLocalTime (snd ttz) (fst ttz))
  mc <- runX $ readDocument [ withRelaxNG (home ++ "/" ++ tdhsRNG)
                              , withRemoveWS yes
                              ] (home ++ "/" ++ tdhsXML) >>>
        (processChildren
         (processChildren (orElse
                           (hasName "rngFile")
                           (processChildren
                            (orElse
                             ((hasName "todolist") >>>
                              (hasAttrValue "name" (== lName)) >>>
                              (processChildren (orElse
                                                ((hasName "ts") >>>
                                                  (processChildren (changeText (\_ -> newTS))))
                                                 returnA)))
                             returnA)
                            )))) >>>
        writeDocument [withIndent yes] (home ++ "/" ++ tdhsXML)
  return ()

dispatch :: (Clops, Maybe Cfg, (UTCTime, TimeZone)) -> IO ()
dispatch (_, Nothing, ttz) = putStrLn "ERROR: unable to configure"
--      Clops    listName     n        add      del   full  ff    dir   urg   all
dispatch ((Clops (Just lName) Nothing  Nothing  False False False False False sa), Just cfg, ttz) =
  gtdShow (rngFile cfg) (fst $ (todoList cfg) ! lName) sa ttz >>
  insertTS lName cfg ttz
dispatch ((Clops (Just lName) Nothing  (Just a) False False False False urg   _ ), Just cfg, ttz) =
  addItem (rngFile cfg) (fst $ (todoList cfg) ! lName) a urg
dispatch ((Clops (Just lName) (Just n) Nothing  True  _     _     _     False sa), Just cfg, ttz) = do
  tsIsOK <- checkTS lName cfg ttz
  if tsIsOK then
    gtdFull (rngFile cfg) (fst $ (todoList cfg) ! lName) n sa ttz >>
    gtdDelete (rngFile cfg) (fst $ (todoList cfg) ! lName) n sa ttz
  else
    putStrLn $ "-- bad timestamp; please do first:   " ++ "td " ++ lName
dispatch ((Clops (Just lName) (Just n) Nothing  False _     False False False sa), Just cfg, ttz) =
  gtdFull (rngFile cfg) (fst $ (todoList cfg) ! lName) n sa ttz
dispatch ((Clops (Just lName) (Just n) Nothing  False False True  False False sa), Just cfg, ttz) =
  gtdOpenInBrowser (rngFile cfg) (fst $ (todoList cfg) ! lName) n sa ttz
dispatch ((Clops (Just lName) (Just n) Nothing  False False False True  False sa), Just cfg, ttz) =
  gtdCD (rngFile cfg) (fst $ (todoList cfg) ! lName) n sa ttz
dispatch ((Clops Nothing _ _ _ _ _ _ _ _), Just cfg, _) =
  gtdShowLists $ todoList cfg
dispatch ((Clops mlName n add del full ff cd urg sa), _, _) = 
  putStrLn "ERROR, contradicting CLOPS:" >>
  putStr "MListName: " >> putStr (show mlName) >> putStr " , n: " >> putStr (show n) >>
  putStr " , add: " >> putStr (show add) >> putStr " , del: " >> putStr (show del) >>
  putStr " , full: " >> putStr (show full) >> putStr " , ff: " >> putStr (show ff) >>
  putStr " , dir: " >> putStr (show cd) >>
  putStr " , urg: " >> putStr (show urg) >>
  putStrLn ""

main :: IO ()
main = execParser opts >>= getConf >>= getTTZ >>= dispatch
  where
    opts = info (helper <*> cloparser)
           ( fullDesc
             <> progDesc "todolist manager"
             <> header "use XML files to track TODOs" )
