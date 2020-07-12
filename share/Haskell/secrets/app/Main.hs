{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import AmkhlvSecrets
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
import qualified Brick.Types as BT
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
import qualified System.Directory as SysDir
import GHC.IO.Handle
import System.Exit
import Data.Maybe
import Data.List (intersperse, sort)
import Text.Printf
import Lib

xdgOpen :: T.Text -> IO ()
xdgOpen t = do
  _ <- createProcess (proc "xdg-open" [T.unpack t])
  return ()

data Error = ParsingError T.Text

allTags :: [Site] -> [T.Text]
allTags sites =
  Sets.toList $ Sets.unions [
  Sets.unions [ Sets.fromList (tags a)
              | a <- accounts site
              ]
  | site <- sites
  ]

  
data SiteAndAccount = SiteAndAccount { nickname :: Nick, siteurl :: URL, account :: Account }

charHintsForNick :: T.Text -> [Site] -> Either Error (Map.Map Char SiteAndAccount)
charHintsForNick s sites =
  case (T.split (=='.') s) of
    [u] ->
      let nas = concat [ map (SiteAndAccount (nick x) (url x)) $ accounts x
                       | x <- filter (T.isInfixOf u . nick) sites
                       ]
      in Right $ Map.fromList $ zip (map C.chr [97..122]) nas
    [u,v] ->
      let (tgs, substr) = (Sets.fromList $ T.split (==',') u,   v)
          nas = concat [
            [SiteAndAccount (nick x) (url x) a
            | a <- filter (Sets.isSubsetOf tgs . Sets.fromList . tags) $ accounts x
            ]
            | x <- filter (T.isInfixOf substr . nick) sites
            ] 
      in Right $ Map.fromList $ zip (map C.chr [97..122]) nas
    _ -> Left $ ParsingError "search terms can contain at most one '.'"

-- GUI

data Name = CommandLine | PasswordEntry  deriving (Ord, Show, Eq)
type ShowTags = Bool
type SearchString = String
data Action = CallRobot TypingMode | ShowAccount deriving Eq
type ShowMoreSecrets = Bool
data Mode =
  MainWin ShowTags 
  | ItemSelector (Map.Map Char SiteAndAccount) Action
  | AccountInfo Account ShowMoreSecrets
  | EnterPassword
  | ErrorMessage [T.Text]
data St =
    St { _mode  :: Mode
       , _sites :: [Site]
       , _cmdLine :: WEdit.Editor String Name
       , _pwdLine :: WEdit.Editor String Name
       }
makeLenses ''St

charHintedItem :: (Char, T.Text) -> BT.Widget Name
charHintedItem p = markup (T.pack [fst p] @? "charhint") <+> str " " <+> markup (snd p @@ fg V.white)

drawMainWin :: St -> ShowTags -> [BT.Widget Name]
drawMainWin st showTags = 
  let listOfWidgets =
        [ str "Ctrl-r to reload"
        , str "Ctrl-t to show/hide tags"
        , str "Ctrl-c to exit"
        , WEdit.renderEditor (str . unlines) True (st^.cmdLine)
        ]
  in if showTags
     then [vBox $ (hBox . map txt . intersperse (T.pack "  ") $ sort $ allTags $ st^.sites) : listOfWidgets]
     else [vBox listOfWidgets]

drawItemSelector :: Map.Map Char SiteAndAccount -> Action -> [BT.Widget Name]
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

drawAccountInfo :: Account -> ShowMoreSecrets -> [BT.Widget Name]
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

drawUI :: St -> [BT.Widget Name]
drawUI st = case (st^.mode) of
  EnterPassword -> [ str "Enter password: " <+> WEdit.renderEditor (str . unlines) True emptyPwdLine ]
  MainWin showTags -> drawMainWin st showTags
  ItemSelector hints action -> drawItemSelector hints action
  AccountInfo acct ssn -> drawAccountInfo acct ssn
  ErrorMessage ts -> [vBox $ markup ("ERROR: " @? "error") : map txt ts]

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr 
    [ (WEdit.editAttr,                   V.white `on` V.blue)
    , (WEdit.editFocusedAttr,            V.black `on` V.yellow)
    , ("red", V.red `on` V.black)
    , ("error", V.white `on` V.red)
    , ("charhint", V.yellow `on` V.black `V.withStyle` V.bold)
    , ("highlighted", V.black `on` V.green)
    , ("selected", V.black `on` V.white)
    , ("unselected", V.white `on` V.black)
    ]

appEvent :: St -> BT.BrickEvent Name e -> BT.EventM Name (BT.Next St)
appEvent st (BT.VtyEvent ev) =
  case (st^.mode) of
    EnterPassword ->
      case ev of
        V.EvKey V.KEnter [] -> do
          let pwd = head $ WEdit.getEditContents $ st^.pwdLine
          args <- IOC.liftIO $ getArgs
          mysites <- IOC.liftIO $ getSecrets (args !! 0) pwd
          IOC.liftIO $ notification ((show $ length $ mysites >>= accounts) ++ " accounts") Nothing
          M.continue $ st & mode .~ MainWin False & sites .~ mysites
        _ -> M.continue =<< BT.handleEventLensed st pwdLine WEdit.handleEditorEvent ev
    MainWin showTags ->
      case ev of
        V.EvKey (V.KChar 'c') [V.MCtrl] -> M.halt st
        V.EvKey (V.KChar 'r') [V.MCtrl] -> do
          let pwd = head $ WEdit.getEditContents $ st^.pwdLine
          args <- IOC.liftIO $ getArgs
          mysites <- IOC.liftIO $ getSecrets (args !! 0) pwd
          IOC.liftIO $ notification ((show $ length $ mysites >>= accounts) ++ " accounts") Nothing
          M.continue $ st & mode .~ MainWin False & sites .~ mysites & cmdLine .~ emptyCmdLine
        V.EvKey (V.KChar 't') [V.MCtrl] -> M.continue $ st & mode .~ MainWin (not showTags)
        V.EvKey V.KEnter [] ->
          case charHintsForNick (T.pack $ head $ WEdit.getEditContents $ st^.cmdLine) (st^.sites) of
            Left (ParsingError t) -> M.continue $ st & mode .~ ErrorMessage [t, "", "press SPACE to continue"]
            Right chfn -> M.continue $ st & mode .~ ItemSelector chfn (CallRobot LoginThenPassword)
        _ -> M.continue =<< BT.handleEventLensed st cmdLine WEdit.handleEditorEvent ev
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
                    IOC.liftIO $ notification ("opening: " ++ T.unpack u) (Just "opening.svg") >> xdgOpen u
                    IOC.liftIO $ typingRobot a acct
                    M.continue $ st & mode .~ MainWin False & cmdLine .~ emptyCmdLine
                  ShowAccount -> 
                    M.continue $ st & mode .~ AccountInfo acct False & cmdLine .~ emptyCmdLine
              Nothing ->
                M.continue st
        V.EvKey V.KEsc [] -> M.continue $ st & mode .~ MainWin False & cmdLine .~ emptyCmdLine
        _ -> M.continue st
    ErrorMessage t ->
      case ev of
        V.EvKey (V.KChar ' ') [] -> retreat
        V.EvKey V.KEsc [] -> retreat
      where retreat = M.continue $ st & mode .~ MainWin False 

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

main :: IO ()
main = do
  notification "starting password manager" (Just "starting.svg")
  _ <- M.defaultMain theApp initialState
  return ()
