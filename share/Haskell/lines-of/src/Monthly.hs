module Monthly where

import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.General.General
import           Graphics.UI.Gtk.Layout.VBox
import           Graphics.UI.Gtk.General.RcStyle
import           Graphics.UI.Gtk.General.CssProvider
import           Graphics.UI.Gtk.General.StyleContext
import           Graphics.UI.Gtk.General.Enums
import qualified Common as COM
import qualified Data.Text as T
import           Control.Monad.IO.Class
import           Control.Concurrent.MVar
import           Data.Maybe
import qualified Data.Time as DT
import qualified Data.Time.Clock as DTClock
import qualified Data.Time.Calendar as DTCal
import qualified Data.Time.LocalTime as DTLoc
import           Data.List.Split
import           GUI
import qualified Database.SQLite.Simple as SQL

inj = COM.inject
  "/home/andrei/.config/amkhlv/lines-of/monthly.css"
  "/home/andrei/.config/amkhlv/lines-of/monthly.sqlite"
  "monthly"
  [  ("message", (20, 1))
  ,  ("months", (20, 1))
  ,  ("days",   (20, 1))
  ,  ("advance", (5, 1))
  ,  ("dismissed", (11, 1))
  ,  ("tmp_note",  (12, 3))
  ,  ("note",      (12, 16))
  -- colname width nlines hide
  ]

spacing = 8

type MainWin = Window
type MasterVBox = VBox
data RegEvent = RegEvent {
  message     :: String
  , months    :: [Int]
  , days      :: [Int]
  , advance   :: Integer
  , dismissed :: DT.Day
  , tmp_note  :: String
  , note      :: String
  , raw :: [String]
  }
data BoxOfEvents = BoxOfEvents { vbox :: MasterVBox, bevs :: [ (Button, RegEvent) ] }

resizeMainWin :: MainWin -> IO ()
resizeMainWin win = windowResize win 300 200

mkRegEvent :: [String] -> RegEvent
mkRegEvent [msg, mths, ds, adv, dis, tmp_nt, nt] =
  RegEvent {
  message = msg
  , months  = case mths of
      "all"  -> [1..12]
      "even" -> [2,4..12]
      "odd"  -> [1,3..11]
      _      -> map read $ splitOn "," mths
  , days    = map read $ splitOn "," ds
  , advance = if adv == "" then 0 else read adv
  , dismissed = let ymd = splitOn "-" dis in
      fromJust $ DT.fromGregorianValid (read $ ymd!!0) (read $ ymd!!1) (read $ ymd!!2)
  , tmp_note = tmp_nt
  , note = nt
  , raw = [msg, mths, ds, adv, dis, tmp_nt, nt]
  }

dayHasEvent :: DT.Day -> RegEvent -> Bool
dayHasEvent day evt = let (y, m, d) = DT.toGregorian day in elem d (days evt) && elem m (months evt)

eventWasDismissedWithinAdvance :: DT.Day -> RegEvent -> Integer -> Bool
eventWasDismissedWithinAdvance day evt adv =
  if (dismissed evt == day) then True
  else if adv == 0 then False
  else eventWasDismissedWithinAdvance (DT.addDays (- 1) day) evt (adv - 1)
eventIsPendingSince :: DT.Day -> RegEvent -> Maybe DT.Day
eventIsPendingSince day evt = eventIsPendingSince' day 0 evt
eventIsPendingSince' :: DT.Day -> Int -> RegEvent -> Maybe DT.Day
eventIsPendingSince' day daysAgo evt =
  if daysAgo > 366 || dismissed evt == day then Nothing
  else if dayHasEvent day evt then
    if eventWasDismissedWithinAdvance day evt (advance evt) then Nothing
    else Just day
  else eventIsPendingSince' (DT.addDays (- 1) day) (daysAgo + 1) evt

refresh :: BoxOfEvents -> MainWin -> IO ()
refresh evbox mainwin = do
  sequence_ [ widgetDestroy (fst bev) | bev <- bevs evbox ]
  prepMasterVBox mainwin (vbox evbox)
  widgetShowAll (vbox evbox)
  resizeMainWin mainwin

dismissEvent :: RegEvent -> DT.Day -> IO ()
dismissEvent ev t = do
  conn <- SQL.open (COM.dbpath inj)
  SQL.withTransaction conn $
    SQL.execute conn (SQL.Query . T.pack $ "UPDATE monthly SET dismissed = ? WHERE message = ?") [DTCal.showGregorian t, message ev]
  SQL.close conn

getNowDay :: IO DT.Day
getNowDay = do
  now <- DTClock.getCurrentTime
  timezone <- DTLoc.getCurrentTimeZone
  let zoneNow = DTLoc.utcToLocalTime timezone now
  return $ DTLoc.localDay zoneNow

manageEvent :: RegEvent -> BoxOfEvents -> MainWin -> IO ()
manageEvent ev evbox mainwin = do
  win <- windowNew
  vb <- vBoxNew False 2
  hb <- hBoxNew False 2
  lbl <- labelNew $ Just (message ev)
  boxPackStart vb lbl PackNatural 1
  dismissBtn   <- buttonNewWithLabel "dismiss"
  editBtn      <- buttonNewWithLabel "edit"
  doNothingBtn <- buttonNewWithLabel "do nothing"
  dismissBtn `on` buttonActivated $ do
    widgetDestroy win
    getNowDay >>= dismissEvent ev
    refresh evbox mainwin
  editBtn `on` buttonActivated $ do
    widgetDestroy win
    collect inj (raw ev) Update (600,20)
    refresh evbox mainwin
  doNothingBtn `on` buttonActivated $ do
    widgetDestroy win
    refresh evbox mainwin
  boxPackStart hb doNothingBtn PackNatural 1
  boxPackEnd   hb editBtn      PackNatural 1
  boxPackEnd   hb dismissBtn   PackNatural 1
  boxPackEnd vb hb PackNatural 1
  containerAdd win vb
  widgetShowAll win
  windowResize win 100 10

prepMasterVBox :: MainWin -> MasterVBox -> IO ()
prepMasterVBox mainwin masterbox = do
  evs <- map mkRegEvent <$> COM.selectAll inj
  t <- getNowDay
  bevs <- sequence $ do
    ev <- evs
    let mp = eventIsPendingSince (DT.addDays (advance ev) t) ev in case mp of
      Just pendingSince -> 
        [ do
            btn <- buttonNewWithLabel $ message ev
            buttonSetRelief btn ReliefNone
            lbl <- binGetChild btn
            (toWidget btn) `set` [widgetMarginLeft := spacing, widgetMarginRight := spacing, widgetMarginBottom := spacing]
            addClass (toWidget btn) "monthly-event-button"
            addClass (toWidget $ fromJust lbl) "monthly-event-label"
            let diff = DTCal.diffDays pendingSince t
            if diff > 3 
              then addClass (toWidget $ fromJust lbl) "monthly-event-label-green"
              else if diff > 0
              then addClass (toWidget $ fromJust lbl) "monthly-event-label-yellow"
              else addClass (toWidget $ fromJust lbl) "monthly-event-label-red"
            return (btn, ev) ]
      Nothing -> []
  sequence_ [ do
                btn `on` buttonActivated $ manageEvent ev (BoxOfEvents masterbox bevs) mainwin
                if i == 0
                  then do (toWidget btn) `set` [widgetMarginTop := spacing]
                          boxPackStart masterbox btn PackNatural 0
                  else boxPackStart masterbox btn PackNatural 0
            | (i, (btn, ev)) <- zip [0..] bevs ]

main :: IO ()
main = do
  _ <- initGUI
  screen <- screenGetDefault
  styleProvider <- cssProviderNew
  cssProviderLoadFromPath styleProvider (COM.csspath inj)
  styleContextAddProviderForScreen (fromJust screen) styleProvider 799
  mainwin <- windowNew
  mainwin `set` [windowTitle := "regular events"]
  mainwin `on` deleteEvent $ liftIO mainQuit >> return False
  masterVBox <- vBoxNew False 0
  addClass (toWidget masterVBox) "monthly-masterbox"
  prepMasterVBox mainwin masterVBox
  containerAdd mainwin masterVBox
  widgetShowAll mainwin
  resizeMainWin mainwin
  mainGUI
