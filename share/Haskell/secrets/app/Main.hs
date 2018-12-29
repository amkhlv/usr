{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Lib
import Prelude hiding (readFile)
import System.Environment
import qualified Control.Monad.IO.Class as IOC
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TLZ
import qualified Data.Char as C
import qualified Text.XML  as TX
import Text.XML.Cursor
import qualified Data.Set as Sets
import qualified Graphics.Vty as V
import Brick (vBox, hBox)
import qualified Brick.Main as M
import qualified Brick.Focus as F
import qualified Brick.Types as T
import Brick.Util (on, fg, bg)
import Brick.Markup (markup, (@?))
import Data.Text.Markup ((@@), fromText)
import qualified Brick.AttrMap as A
import Brick.Widgets.Core ((<+>), (<=>), str, txt)
import qualified Brick.Widgets.Center as WCenter
import qualified Brick.Widgets.Edit   as WEdit
import Lens.Micro
import Lens.Micro.TH
import System.Process
import GHC.IO.Handle
import System.Exit
import Data.Maybe
import Data.List (intersperse)
import Text.Printf

xdgOpen :: T.Text -> IO ()
xdgOpen t = do
  _ <- createProcess (proc "xdg-open" [T.unpack t])
  return ()

typeText :: T.Text -> IO ()
typeText t = do
  (mstdin1, _, _, hndl1) <- createProcess
    (proc "/usr/local/lib/amkhlv/xvkbd-helper.sh" []){std_in = CreatePipe, std_err = CreatePipe}
  let stdin = fromJust mstdin1 in hPutStr stdin (T.unpack t) >> hFlush stdin
  waitForProcess hndl1
  return ()

notification :: String -> IO ()
notification x = do
  _ <- createProcess (proc "notify-send" ["-t", "1500", x])
  return ()

typingRobot :: TypingMode -> Account -> IO ()
typingRobot LoginThenPassword acct = do
  notification "ready to type login"
  typeText $ login acct
  notification "typing login"
  typeText $ password acct
  notification "typing password"
typingRobot PasswordThenLogin acct = do
  notification "ready to type password"
  typeText $ password acct
  notification "typing password"
  typeText $ login acct
  notification "typing login"
typingRobot JustPassword acct = do
  notification "ready to typ password"
  typeText $ password acct
  notification "typing password"

data Account = Account
  { login :: T.Text
  , password :: T.Text
  , changedOn :: [T.Text]
  , expiringOn :: [T.Text]
  , description :: [T.Text]
  , notes :: [T.Text]
  , loginChallenge :: [T.Text]
  , forgotPasswordChallenge :: [T.Text]
  , secretNotes :: [T.Text]
  , tags :: [T.Text]
  } deriving Show

getValues :: T.Text -> Cursor -> [T.Text]
getValues x cur =
  child >=> checkName (\nm -> x == TX.nameLocalName nm) >=> child >=> content $ cur

getAccount :: Cursor -> Account
getAccount cur =
  Account
  { login =
    head
    $ laxAttribute "login" cur
  , password =
    head
    $ child >=> checkName (\nm -> "password" == TX.nameLocalName nm) >=> child >=> content
    $ cur
  , changedOn =
    laxAttribute (T.pack "expires") cur
  , expiringOn =
    laxAttribute (T.pack "changed") cur
  , description =
    getValues "description" cur
  , notes =
    getValues "notes" cur
  , loginChallenge =
    getValues "login_challenge" cur
  , forgotPasswordChallenge =
    getValues "forgot_password_challenge" cur
  , secretNotes =
    getValues "secret_notes" cur
  , tags =
    child >=> checkName (\nm -> "tags" == TX.nameLocalName nm) >=> child >=> child >=> content
    $ cur
  }

type Nick = T.Text
type URL = T.Text
data Site = Site { nick :: Nick, url :: URL, accounts :: [Account] } deriving Show

getSite :: Cursor -> Site
getSite cur =
  Site
  (head $ laxAttribute (T.pack "nick") cur)
  (head $ laxAttribute (T.pack "url") cur)
  (map getAccount $ (child >=> checkName (\nm -> "account" == TX.nameLocalName nm)) cur)

getData :: Cursor -> [Site]
getData cur = map getSite $ (child >=> checkName (\nm -> "site" == TX.nameLocalName nm)) cur

readDataFromFile :: IO [Site]
readDataFromFile = do
  doc <- TX.readFile TX.def "example.xml"
  let cur = fromDocument doc
      sites = map getSite $ (child >=> anyElement) $ head $ (child >=> anyElement) cur
  return sites

allTags :: [Site] -> [T.Text]
allTags sites =
  Sets.toList $ Sets.unions [
  Sets.unions [ Sets.fromList (tags a)
              | a <- accounts site
              ]
  | site <- sites
  ]

  
data SiteAndAccount = SiteAndAccount { nickname :: Nick, siteurl :: URL, account :: Account }

charHintsForNick :: T.Text -> [Site] -> Map.Map Char SiteAndAccount
charHintsForNick s sites = 
  let (tgs, substr) = case (T.split (=='.') s) of
        [u] -> (Sets.empty, u)
        [u,v] -> (Sets.fromList $ T.split (==',') u,   v)
      nas = concat [
        [SiteAndAccount (nick x) (url x) a
        | a <- filter (Sets.isSubsetOf tgs . Sets.fromList . tags) $ accounts x
        ]
        | x <- filter (T.isInfixOf substr . nick) sites
        ]
  in Map.fromList $ zip (map C.chr [97..122]) nas

-- GUI

data Name = CommandLine | PasswordEntry  deriving (Ord, Show, Eq)
type ShowTags = Bool
type SearchString = String
data TypingMode = LoginThenPassword | PasswordThenLogin | JustPassword deriving Eq
data Action = CallRobot TypingMode | ShowAccount deriving Eq
type ShowMoreSecrets = Bool
data Mode =
  MainWin ShowTags 
  | ItemSelector (Map.Map Char SiteAndAccount) Action
  | AccountInfo Account ShowMoreSecrets
  | EnterPassword
  | ErrorMessage T.Text
data St =
    St { _mode  :: Mode
       , _sites :: [Site]
       , _cmdLine :: WEdit.Editor String Name
       , _pwdLine :: WEdit.Editor String Name
       }
makeLenses ''St

charHintedItem :: (Char, T.Text) -> T.Widget Name
charHintedItem p = markup (T.pack [fst p] @? "charhint") <+> str " " <+> markup (snd p @@ fg V.white)

drawMainWin :: St -> ShowTags -> [T.Widget Name]
drawMainWin st showTags = 
  let listOfWidgets =
        [ str "Ctrl-r to reload"
        , str "Ctrl-t to show/hide tags"
        , str "Ctrl-c to exit"
        , WEdit.renderEditor (str . unlines) True (st^.cmdLine)
        ]
  in if showTags
     then [vBox $ (hBox . map txt . intersperse (T.pack "  ") $ allTags $ st^.sites) : listOfWidgets]
     else [vBox listOfWidgets]

drawItemSelector :: Map.Map Char SiteAndAccount -> Action -> [T.Widget Name]
drawItemSelector hints action =
  let f b x = if b then markup (T.pack x @? "selected") else markup (T.pack x @? "unselected")
      top = hBox [ f (action == CallRobot LoginThenPassword) "LoginThenPassword"
                 , str " "
                 , f (action == CallRobot JustPassword) "JustPassword"
                 , str " "
                 , f (action == CallRobot PasswordThenLogin) "PasswordThenLogin"
                 , str " "
                 , f (action == ShowAccount) "ShowSecretData"
                 ]
      slctr = vBox [vBox $ [ charHintedItem (fst x, login $ account $ snd x)
                             <+>
                             str " "
                             <+>
                             markup (nickname (snd x) @? "highlighted")
                             <+>
                             str "("
                             <+>
                             (txt . siteurl . snd) x
                             <+>
                             str ")"
                             <+>
                             markup (T.pack " TAGS: " @@ fg V.blue)
                             <+>
                             str (show $ tags $ account $ snd x)
                           , txt (T.concat
                                  [ T.pack "  "
                                  , (T.intercalate (T.pack "\n") . description . account . snd) x
                                  ])
                           , txt (T.concat
                                  [ T.pack "  "
                                  , (T.intercalate (T.pack "\n") . notes . account . snd) x
                                  ])
                           ] ++ if ((secretNotes . account . snd) x
                                    ++ (loginChallenge . account . snd) x
                                    ++ (forgotPasswordChallenge . account . snd) x) == []
                                then []
                                else [ markup ("  secret info available" @? "red") ]
                   | x <- Map.assocs hints]
  in [vBox [top, slctr]]

drawAccountInfo :: Account -> ShowMoreSecrets -> [T.Widget Name]
drawAccountInfo acct ssn = [v] where
  v = vBox $ concat
    [ [ str (printf "password changed on %s" $ T.unpack t) | t <- changedOn acct ]
    , [ str (printf "password expires on %s" $ T.unpack t) | t <- expiringOn acct ]
    , [ markup ("login challenge: " @? "charhint") <+> markup (t @? "red")
      | t <- loginChallenge acct
      ]
    , if ssn
      then [ markup ("\"forgot password\" challenge: " @? "charhint") <+> markup (t @? "red")
           | t <- forgotPasswordChallenge acct
           ]
      else [ markup ("\"forgot password\" challenge" @? "charhint") <+> str " on file, press SPACE to show"
           | t <- forgotPasswordChallenge acct
           ]
    , if secretNotes acct == []
      then []
      else if ssn
           then [ markup (t @? "red")
                | t <- secretNotes acct
                ]
           else [ str "exists "
                  <+>
                  markup ("secret note: " @? "charhint")
                  <+>
                  str " --- press SPACE to show"
                ]
    ]

drawUI :: St -> [T.Widget Name]
drawUI st = case (st^.mode) of
  EnterPassword -> [ str "Enter password: " <+> WEdit.renderEditor (str . unlines) True emptyPwdLine ]
  MainWin showTags -> drawMainWin st showTags
  ItemSelector hints action -> drawItemSelector hints action
  AccountInfo acct ssn -> drawAccountInfo acct ssn
  ErrorMessage t -> [txt "ERROR: " <+> txt t]

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr 
    [ (WEdit.editAttr,                   V.white `on` V.blue)
    , (WEdit.editFocusedAttr,            V.black `on` V.yellow)
    , ("red", V.red `on` V.black)
    , ("charhint", V.yellow `on` V.black `V.withStyle` V.bold)
    , ("highlighted", V.black `on` V.green)
    , ("selected", V.black `on` V.white)
    , ("unselected", V.white `on` V.black)
    ]

appEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appEvent st (T.VtyEvent ev) =
  case (st^.mode) of
    EnterPassword ->
      case ev of
        V.EvKey V.KEnter [] -> do
          let pwd = head $ WEdit.getEditContents $ st^.pwdLine
          mysites <- IOC.liftIO $ getSecrets pwd
          IOC.liftIO $ notification $ (show $ length $ mysites >>= accounts) ++ " accounts" 
          M.continue $ st & mode .~ MainWin False & sites .~ mysites
        _ -> M.continue =<< T.handleEventLensed st pwdLine WEdit.handleEditorEvent ev
    MainWin showTags ->
      case ev of
        V.EvKey (V.KChar 'c') [V.MCtrl] -> M.halt st
        V.EvKey (V.KChar 'r') [V.MCtrl] -> do
          let pwd = head $ WEdit.getEditContents $ st^.pwdLine
          mysites <- IOC.liftIO $ getSecrets pwd
          IOC.liftIO $ notification $ (show $ length $ mysites >>= accounts) ++ " accounts" 
          M.continue $ st & mode .~ MainWin False & sites .~ mysites & cmdLine .~ emptyCmdLine
        V.EvKey (V.KChar 't') [V.MCtrl] -> M.continue $ st & mode .~ MainWin (not showTags)
        V.EvKey V.KEnter [] ->
          let s = head $ WEdit.getEditContents $ st^.cmdLine
          in M.continue $ st & mode .~ ItemSelector
             (charHintsForNick (T.pack s) (st^.sites))
             (CallRobot LoginThenPassword)
        _ -> M.continue =<< T.handleEventLensed st cmdLine WEdit.handleEditorEvent ev
    AccountInfo acct ssn ->
      case ev of
        V.EvKey (V.KChar ' ') [] -> M.continue $ st & mode .~ AccountInfo acct (not ssn)
        V.EvKey V.KEsc [] -> M.continue $ st & mode .~ MainWin False & cmdLine .~ emptyCmdLine
        _ -> M.continue st
    ItemSelector hints action ->
      case ev of
        V.EvKey (V.KChar '\t') [] ->
          let newaction = case action of
                CallRobot LoginThenPassword -> CallRobot JustPassword
                CallRobot JustPassword -> CallRobot PasswordThenLogin
                CallRobot PasswordThenLogin -> ShowAccount
                ShowAccount -> CallRobot LoginThenPassword
          in M.continue $ st & mode .~ ItemSelector hints newaction
        V.EvKey (V.KChar c) [] ->
          case Map.lookup c hints of
            Just (SiteAndAccount _ u acct) -> 
              case action of
                CallRobot a -> do
                  IOC.liftIO $ typingRobot a acct
                  M.continue $ st & mode .~ MainWin False & cmdLine .~ emptyCmdLine
                ShowAccount -> 
                  M.continue $ st & mode .~ AccountInfo acct False & cmdLine .~ emptyCmdLine
            Nothing -> case Map.lookup (C.toLower c) hints of
              Just (SiteAndAccount _ u acct) -> 
                case action of
                  CallRobot a -> do
                    IOC.liftIO $ notification ("opening: " ++ T.unpack u)  >> xdgOpen u 
                    IOC.liftIO $ typingRobot a acct
                    M.continue $ st & mode .~ MainWin False & cmdLine .~ emptyCmdLine
                  ShowAccount -> 
                    M.continue $ st & mode .~ AccountInfo acct False & cmdLine .~ emptyCmdLine
              Nothing ->
                M.continue st
        V.EvKey V.KEsc [] -> M.continue $ st & mode .~ MainWin False & cmdLine .~ emptyCmdLine
        _ -> M.continue st
appEvent st _ = M.continue st

theApp :: M.App St e Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = (\st -> M.showCursorNamed CommandLine)
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

emptyCmdLine = WEdit.editor CommandLine (Just 1) ""
emptyPwdLine = WEdit.editor PasswordEntry (Just 1) ""

initialState :: St
initialState = St EnterPassword [] emptyCmdLine emptyPwdLine

getSecrets :: String -> IO [Site]
getSecrets pwd = do
  args <- getArgs
  (Just stdin, Just stdout, Just stderr, hndl) <- createProcess
    (proc "gpg" ["--batch", "--passphrase-fd", "0", "--decrypt", args !! 0])
    { std_in  = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    }
  hPutStr stdin pwd >> hFlush stdin
  contents <- hGetContents stdout
  let txt = T.pack contents
      doc = TX.parseText_ TX.def (TLZ.pack contents)
      cur = fromDocument doc
  return $ map getSite $ (child >=> anyElement) $ head $ (child >=> anyElement) cur  

main :: IO ()
main = do
  notification "starting password manager"
  _ <- M.defaultMain theApp initialState
  return ()
