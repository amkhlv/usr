{-# LANGUAGE Arrows #-}

module Main where
import Options.Applicative
import Text.XML.HXT.Core
import Text.XML.HXT.RelaxNG
import Text.XML.HXT.Arrow.Pickle
import Text.XML.HXT.Arrow.XmlArrow
import qualified Text.XML.HXT.DOM.XmlNode as DOM
import Data.Text (Text, pack, unpack, split)
import Data.List
import Control.Monad
import System.Console.ANSI
import qualified Data.Tree.Class as DTC
import Data.Map
import Data.Maybe
import System.Directory
import System.Process
import System.IO
import Data.Time
import Graphics.UI.Gtk hiding (Action, backspace)
import Graphics.UI.Gtk.Layout.Table
import Data.IORef
import Control.Monad.IO.Class

tdhsXML = ".config/amkhlv/td.hs.xml"
tdhsRNG = ".config/amkhlv/td.hs.rng"

data Clops = Clops
             { listName :: Maybe String
             , n :: Maybe Int
             , add :: Maybe String
             , del :: Bool
             , edt :: Bool
             , full :: Bool
             , ff :: Bool
             , dir :: Bool
             , urg :: Bool
             , showAll :: Bool
             , newListName :: Maybe String
             , newXMLFile  :: Maybe String
             }

cloparser :: Parser Clops
cloparser = Clops
  <$> optional ( argument str (metavar "LISTNAME"))
  <*> optional ( option auto ( short 'n' <> metavar "N" <> help "Select entry"))
  <*> optional ( option str ( short 'a' <> metavar "A" <> help "Add"))
  <*> switch ( short 'd' <> help "Delete" )
  <*> switch ( short 'e' <> help "Edit" )
  <*> switch ( short 'f' <> help "Whether to give full description" )
  <*> switch ( long "ff" <> help "Open URL in Firefox")
  <*> switch ( long "dir" <> help "print path to the project directory")
  <*> switch ( long "urg" <> help "urgent flag")
  <*> switch ( long "all" <> help "show all items included postponed")
  <*> optional ( option str (long "newlist" <> metavar "NL" <> help "New list"))
  <*> optional ( option str (long "file" <> metavar "NF" <> help "XML file for new list"))

data Cfg = Cfg
  { rngFile :: String
  , todoList :: TodoListMap
  } deriving (Show, Eq)

instance XmlPickler Cfg where xpickle = xpCfg
xpCfg :: PU Cfg
xpCfg =
  xpElem "config" $
  xpWrap ( \(r,t) -> Cfg r t , \cf -> (rngFile cf, todoList cf) ) $
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
  xpWrap ( \(t,p) -> (p,t) , \x -> (snd x, fst x) ) $
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
  xpWrap ( \ (ni, dl, desc, url, dir, pp, urgent) -> TodoItem ni dl desc url dir pp urgent
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
  putStr nick
  case mdesc of
    Just desc -> setSGR [SetColor Foreground Vivid Green] >> (putStr " …") >> setSGR [Reset]
    Nothing -> return ()
  case mdl of
    Just dl -> setSGR [SetColor Foreground Vivid Red] >> (putStr  $ " [" ++ dl ++ "]" )  >> setSGR [Reset]
    Nothing -> return ()
  putStrLn ""
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
    let n = if sa then n0 - 1 else (ns ttz items) !! (n0 - 1)
    putStrLn "delete this?"
    c <- getChar
    case c of
      'y' -> runX $
        readTodoListXML r x  >>>
        (processChildren (processChildren isElem)) >>>
        (processChildren (changeChildren (\xmls -> (Data.List.take n xmls) ++ (Data.List.drop (n + 1) xmls)))) >>>
        writeDocument [withIndent yes] x
      _ -> return []
    return ()

gtdFull :: String -> String -> Int -> Bool -> (UTCTime, TimeZone) -> IO ()
gtdFull r x n0 sa ttz = do
  items <- getTodoItems r x
  let n = if sa then n0 - 1 else (ns ttz items) !! (n0 - 1)
  case (items !! n) of -- TodoItem nick deadline description url dir
    Just (TodoItem a mdl mdsc murl mdir mpp murgent) ->
      putStrLn "" >> setSGR [Reset, SetUnderlining SingleUnderline] >> putStrLn a >> setSGR [ Reset ] >>
      do
        case mpp of
          (Just pp) -> setSGR [SetColor Foreground Vivid Yellow] >> putStr "● " >> setSGR [Reset] >>
            putStr "postponed until " >> setSGR [SetColor Foreground Vivid Green] >> putStrLn pp >> setSGR [Reset]
          _ -> return ()
        case mdl of
          (Just dl) -> setSGR [SetColor Foreground Vivid Yellow] >> putStr "● " >> setSGR [Reset] >> putStr "deadline: " >>
            setSGR [SetColor Foreground Vivid Red] >> putStrLn dl >> setSGR [Reset]
          _ -> return ()
        case mdsc of
          (Just dsc) -> putStrLn dsc
          _ -> return ()
        case murl of
          (Just url) -> setSGR [SetColor Foreground Vivid Blue, SetUnderlining SingleUnderline] >> putStrLn url >> setSGR [Reset]
          _ -> return ()
        case mdir of
          (Just dir) -> setSGR [SetColor Foreground Vivid Yellow] >> putStrLn dir >> setSGR [Reset]
          _ -> return ()
        putStrLn ""
    _ -> putStrLn "ERROR"

gtdOpenInBrowser :: String -> String -> Int -> Bool -> (UTCTime, TimeZone) -> IO ()
gtdOpenInBrowser r x n0 sa ttz = do
  items <- getTodoItems r x
  let n = if sa then n0 - 1 else ns ttz items !! (n0 - 1)
  case items !! n of
    Just (TodoItem a _ _ (Just url) _ _ _) -> spawnCommand ("firefox '" ++ url ++ "'") >> putStrLn "opening in Firefox"
    _ -> putStrLn "-- no URL to open"

gtdCD :: String -> String -> Int -> Bool -> (UTCTime, TimeZone) -> IO ()
gtdCD r x n0 sa ttz = do
  home <- getHomeDirectory
  current <- getCurrentDirectory
  items <- getTodoItems r x
  let n = if sa then n0 - 1 else ns ttz items !! (n0 - 1)
  case items !! n of
    Just (TodoItem a _ _ _ (Just dir) _ _) -> putStr (home ++ "/" ++ dir)
    _ -> putStr current

gtdShowLists :: TodoListMap -> IO ()
gtdShowLists tdlm = sequence_ [ putStr k | k <- intersperse " " (keys tdlm)] >> putStrLn ""

checkTS :: String -> Cfg -> (UTCTime, TimeZone) -> IO Bool
checkTS lName cfg ttz = do
  let expectedTS = formatTime defaultTimeLocale "%s" (fst ttz)
  return $ case snd (todoList cfg ! lName) of
             Just ts -> (read expectedTS :: Int) < ((read ts :: Int) + 60)
             Nothing -> False

insertTS :: String -> Cfg -> (UTCTime, TimeZone) -> IO ()
insertTS lName cfg ttz = do
  home <- getHomeDirectory
  let  newTS = formatTime defaultTimeLocale "%s" (fst ttz)
  mc <- runX $ readDocument [ withRelaxNG (home ++ "/" ++ tdhsRNG)
                              , withRemoveWS yes
                              ] (home ++ "/" ++ tdhsXML) >>>
        processChildren
         (processChildren (orElse
                           (hasName "rngFile")
                           (processChildren
                            (orElse
                             (hasName "todolist" >>>
                              hasAttrValue "name" (== lName) >>>
                              processChildren (none `Text.XML.HXT.Core.when` hasName "ts") >>>
                               changeChildren
                                 (\xmls -> DOM.NTree (XTag (mkName "ts") []) [DOM.NTree (XText newTS) []]:xmls)
                              )
                              returnA)))) >>>
        writeDocument [withIndent yes] (home ++ "/" ++ tdhsXML)
  return ()

insertNewList :: String -> String -> Cfg -> IO ()
insertNewList name filename cfg = do
  home <- getHomeDirectory
  absPath <- makeAbsolute filename
  runX $ readDocument [ withRelaxNG (home ++ "/" ++ tdhsRNG)
                              , withRemoveWS yes
                              ] (home ++ "/" ++ tdhsXML) >>>
        processChildren
         (processChildren (orElse
                           (hasName "rngFile")
                           (changeChildren
                            (\xmls -> DOM.NTree
                                       (XTag
                                        (mkName "todolist")
                                        [DOM.NTree (XAttr (mkName "name")) [DOM.NTree (XText name) []]])
                                       [DOM.NTree
                                        (XTag (mkName "path") [])
                                          [DOM.NTree (XText absPath) []]]
                                      :xmls
                            ))))
         >>>
         writeDocument [withIndent yes] (home ++ "/" ++ tdhsXML)
  runX $ root [] [mkelem "todolist" [] []] >>> writeDocument [withIndent yes] filename
  return ()


replaceItem :: String -> String -> TodoItem -> TodoItem -> IO ()
replaceItem r x oldItem newItem = do
  runX $
    readTodoListXML r x
    >>>
    processChildren
      (processChildren
        (orElse
          (hasName "todo"
           >>>
           hasAttrValue "nick" (== nick oldItem)
           >>>
           case description oldItem of
             Nothing -> returnA
             Just txt -> processChildren (hasName "description" >>> hasText (== txt))
           >>>
           setChildren (DTC.getChildren $ head (DTC.getChildren (pickleDoc xpTodoItem newItem)))
           )
          returnA
          ))
    >>>
    writeDocument [withIndent yes] x
  return ()

gtdEdit :: String -> String -> Int -> Bool -> (UTCTime, TimeZone) -> IO ()
gtdEdit r x n0 sa ttz = do
  items <- getTodoItems r x
  let n = if sa then n0 - 1 else ns ttz items !! (n0 - 1)
  let item = fromJust $ items !! n
  pp <- newIORef $ postponed item
  dl <- newIORef $ deadline item
  --
  void initGUI
  window <- windowNew
  set window [ windowTitle         := "Edit Entry"
             , windowDefaultWidth  := 400
             , windowDefaultHeight := 200 ]
  --
  vboxMain <- vBoxNew False 3
  hboxMain <- hBoxNew True  3
  vboxL <- vBoxNew False 3
  vboxR <- vBoxNew False 3
  hboxBottom <- hBoxNew True 3
  --
  mainTbl <- tableNew 2 6 False
  --
  btnUnselDL <- buttonNewWithLabel $ case deadline item of
    Nothing -> "No deadline"
    Just x -> "Deadline: " ++ x
  calDL <- calendarNew
  calendarClearMarks calDL
  case deadline item of
    Just ymd -> let [yyyy, mm, dd] = Data.List.map unpack $ Data.Text.split (== '-') (pack ymd) in do
      calendarSelectMonth calDL (read mm - 1) (read yyyy)
      calendarSelectDay calDL (read dd)
    Nothing -> return ()
  onDaySelected calDL $ do
    (nY, nM, nD) <- calendarGetDate calDL
    let mm = if nM < 9  then "0" ++ show (nM + 1) else show (nM + 1)
    let dd = if nD < 10 then "0" ++ show nD else show nD
    buttonSetLabel btnUnselDL $ "Deadline: " ++ show nY ++ "-" ++ mm ++ "-" ++ dd
    writeIORef dl (Just $ show nY ++ "-" ++ mm ++ "-" ++ dd)
  --
  btnUnselPP <- buttonNewWithLabel $ case postponed item of
    Nothing -> "Not postponed"
    Just x -> "Postponed until " ++ x
  calPP <- calendarNew
  calendarClearMarks calPP
  case postponed item of
    Just ymd -> let [yyyy, mm, dd] = Data.List.map unpack $ Data.Text.split (== '-') (pack ymd) in do
      calendarSelectMonth calPP (read mm - 1) (read yyyy)
      calendarSelectDay calPP (read dd)
    Nothing -> return ()
  onDaySelected calPP $ do
    (nY, nM, nD) <- calendarGetDate calPP
    let mm = if nM < 9  then "0" ++ show (nM + 1) else show (nM + 1)
    let dd = if nD < 10 then "0" ++ show nD else show nD
    buttonSetLabel btnUnselPP $ "Postponed until " ++ show nY ++ "-" ++ mm ++ "-" ++ dd
    writeIORef pp (Just $ show nY ++ "-" ++ mm ++ "-" ++ dd)
  --
  lblNick <- labelNew (Just $ nick item)
  labelSetAttributes lblNick [AttrScale 0 (-1) 1.2]
  --
  lblDesc <- labelNew (Just "description")
  txtvDesc <- textViewNew
  tvBuf <- textViewGetBuffer txtvDesc
  let txt0 = fromMaybe "" $ description item 
  textBufferSetText tvBuf txt0
  tableAttachDefaults mainTbl lblDesc  0 1  1 2
  tableAttachDefaults mainTbl txtvDesc 2 3  1 2
  --
  lblURL <- labelNew (Just "URL")
  entryURL <- entryNew
  entrySetText entryURL $ fromMaybe "" $ url item
  tableAttachDefaults mainTbl lblURL   0 1  2 3
  tableAttachDefaults mainTbl entryURL 2 3  2 3
  --
  lblDir <- labelNew (Just "Folder")
  entryDir <- entryNew
  entrySetText entryDir $ fromMaybe "" $ directory item
  tableAttachDefaults mainTbl lblDir   0 1  3 4
  tableAttachDefaults mainTbl entryDir 2 3  3 4
  --
  btnUrg <- toggleButtonNewWithLabel "Urgent"
  toggleButtonSetActive btnUrg $ isJust $ urgent item
  tableAttachDefaults mainTbl btnUrg 0 3 4 5
  --
  btnOK <- buttonNew
  set btnOK [ buttonLabel := "OK" ]
  btnOK `on` buttonActivated $ do
    (i0,i1) <- (,) <$> textBufferGetIterAtOffset tvBuf 0 <*> textBufferGetIterAtOffset tvBuf (- 1)
    newDesc <- textBufferGetText tvBuf i0 i1 True
    newURL  <- entryGetText entryURL
    newDir  <- entryGetText entryDir
    newDL   <- readIORef dl
    newPP   <- readIORef pp
    newUrg  <- toggleButtonGetActive btnUrg
    let newItem = TodoItem
                  (nick item)
                  newDL
                  (if newDesc == "" then Nothing else Just newDesc)
                  (if newURL == "" then Nothing else Just newURL)
                  (if newDir == "" then Nothing else Just newDir)
                  newPP
                  (if newUrg then Just "" else Nothing)
    replaceItem r x item newItem
    mainQuit
  --
  btnCancel <- buttonNew
  set btnCancel [ buttonLabel := "Cancel" ]
  btnCancel `on` buttonActivated $ mainQuit
  --
  btnUnselPP `on` buttonActivated $ do
    calendarClearMarks calPP
    buttonSetLabel btnUnselPP "Not postponed"
    writeIORef pp Nothing
  btnUnselDL `on` buttonActivated $ do
    calendarClearMarks calDL
    buttonSetLabel btnUnselDL "No Deadline"
    widgetQueueDraw calDL
    writeIORef dl Nothing
  --
  containerAdd vboxMain lblNick
  containerAdd vboxMain mainTbl
  containerAdd vboxL btnUnselPP
  containerAdd vboxL calPP
  containerAdd hboxMain vboxL
  containerAdd vboxR btnUnselDL
  containerAdd vboxR calDL
  containerAdd hboxMain vboxR
  containerAdd vboxMain hboxMain
  containerAdd hboxBottom btnOK
  containerAdd hboxBottom btnCancel
  containerAdd vboxMain hboxBottom
  containerAdd window vboxMain
  window `on` deleteEvent $ do 
    liftIO mainQuit
    return False
  widgetShowAll window
  mainGUI
  return ()


getConf :: IO (Maybe Cfg)
getConf = do
  home <- getHomeDirectory
  mc <- runX $ readDocument [ withRelaxNG (home ++ "/" ++ tdhsRNG)
                            , withRemoveWS yes
                            ] (home ++ "/" ++ tdhsXML) >>>
        getChildren >>> isElem >>> processChildren isElem >>^ unpickleDoc xpCfg
  return $ head mc

getTTZ :: IO (UTCTime, TimeZone)
getTTZ = (,) <$> getCurrentTime <*> getCurrentTimeZone

dispatch :: Clops -> Maybe Cfg -> (UTCTime, TimeZone) -> IO ()
dispatch _ Nothing ttz = putStrLn "ERROR: unable to configure"
--      Clops    listName     n        add      del   edt   full  ff    dir   urg   all
dispatch
  (Clops (Just lName) Nothing  Nothing  False False False False False False sa Nothing Nothing)
  (Just cfg)
  ttz
  =
  gtdShow (rngFile cfg) (fst $ todoList cfg ! lName) sa ttz >>
  insertTS lName cfg ttz
dispatch
  (Clops (Just lName) Nothing  (Just a) False edt   False False False urg   _ Nothing Nothing)
  (Just cfg)
  ttz
  =
  addItem (rngFile cfg) (fst $ todoList cfg ! lName) a urg
dispatch
  (Clops (Just lName) (Just n) Nothing  True  _     _     _     _     False sa Nothing Nothing)
  (Just cfg)
  ttz
  =
  do
    tsIsOK <- checkTS lName cfg ttz
    if tsIsOK then
      gtdFull   (rngFile cfg) (fst $ todoList cfg ! lName) n sa ttz >>
      gtdDelete (rngFile cfg) (fst $ todoList cfg ! lName) n sa ttz
    else
      putStrLn $ "-- bad timestamp; please do first:   " ++ "td " ++ lName
dispatch
  (Clops (Just lName) (Just n) Nothing  False edt   False False False _     sa Nothing Nothing)
  (Just cfg)
  ttz
  =
  gtdEdit (rngFile cfg) (fst $ todoList cfg ! lName) n sa ttz
dispatch
  (Clops (Just lName) (Just n) Nothing  False False True  False False False sa Nothing Nothing)
  (Just cfg)
  ttz
  =
  gtdFull (rngFile cfg) (fst $ todoList cfg ! lName) n sa ttz
dispatch
  (Clops (Just lName) (Just n) Nothing  False False False True  False False sa Nothing Nothing)
  (Just cfg)
  ttz
  =
  gtdOpenInBrowser (rngFile cfg) (fst $ todoList cfg ! lName) n sa ttz
dispatch
  (Clops (Just lName) (Just n) Nothing  False False False False True  False sa Nothing Nothing)
  (Just cfg)
  ttz
  =
  gtdCD (rngFile cfg) (fst $ todoList cfg ! lName) n sa ttz
dispatch (Clops Nothing _ _ _ _ _ _ _ _ _ Nothing Nothing) (Just cfg) _ = gtdShowLists $ todoList cfg
dispatch (Clops mlName n add del edt full ff cd urg sa (Just newlist) (Just newxml)) (Just cfg) _ =
  insertNewList newlist newxml cfg
dispatch (Clops mlName n add del edt full ff cd urg sa newlist newxml) _ _ =
  putStrLn "ERROR, contradicting CLOPS:" >>
  putStr "MListName: " >> putStr (show mlName) >> putStr " , n: " >> putStr (show n) >>
  putStr " , add: " >> putStr (show add) >> putStr " , del: " >> putStr (show del) >>
  putStr " , full: " >> putStr (show full) >> putStr " , ff: " >> putStr (show ff) >>
  putStr " , dir: " >> putStr (show cd) >>
  putStr " , urg: " >> putStr (show urg) >>
  putStrLn ""

main :: IO ()
main = join $ dispatch <$> execParser opts <*> getConf <*> getTTZ
  where
    opts = info (helper <*> cloparser)
           ( fullDesc
             <> progDesc "todolist manager"
             <> header "use XML files to track TODOs" )

