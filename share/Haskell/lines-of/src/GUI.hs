module GUI where

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
import           System.Process
import           GHC.IO.Handle

rowsetBatchSize = 12

getAllText :: TextView -> IO String
getAllText tv = do
  bf <- textViewGetBuffer tv
  s <- textBufferGetStartIter bf
  e <- textBufferGetEndIter bf
  textBufferGetText bf s e True

truncate :: String -> Int -> String
truncate x i =
  let t = T.pack x
      repl '\n' = '⏎'
      repl c    = c
      preResult = T.map repl $ T.take i t
  in
    if preResult == t then x else T.unpack $ preResult `T.append` T.pack "…"

addClass :: Widget -> String -> IO ()
addClass w x = do
  styleCtxt <- widgetGetStyleContext w
  styleContextAddClass styleCtxt x

data Role = Collect | Show | Update | Delete
type Width      = Int
type Height     = Int

collect :: COM.Injection -> [String] -> Role -> (Width, Height) -> IO ()
collect inj prefills role (wdth, ht) = do
  win <- windowNew
  vb  <- vBoxNew False 2
  controlBox <- hBoxNew False 2
  gr  <- tableNew (1 + length prefills) 3 False
  lines <- sequence [
    do
      v <- textViewNew
      addClass (toWidget v) "line-entry"
      b <- textViewGetBuffer v
      textBufferSetText b $ prefill
      return v
    | prefill <- prefills
    ]
  scrollWins <- sequence [
    do
      ha <- textViewGetHadjustment line
      va <- textViewGetVadjustment line
      sw <- scrolledWindowNew (Just ha) (Just va)
      widgetSetSizeRequest sw wdth $ ht * nlines
      containerAdd sw $ line
      return sw
    | (line, nlines) <- zip lines [x | (_, (_,x)) <- COM.rowspecs inj]
    ]
  sequence_ [
    do
      lbl <- labelNew $ Just ttl
      addClass (toWidget lbl) "linii-label-colname"
      addClass (toWidget lbl) $ "linii-label-col-" ++ ttl
      vb <- vBoxNew False 2
      boxPackStart vb (scrollWins !! i) PackNatural 1
      tableAttach gr lbl 0 1 i (i + 1) [Expand, Fill] [Shrink] 0 0
      tableAttach gr vb  1 2 i (i + 1) [Expand, Fill] [Shrink] 0 0
    | (i, (ttl, _)) <- zip [0..] $ COM.rowspecs inj
    ]
  case role of
    Collect -> do
      collectButton <- buttonNewWithLabel "collect"
      collectButton `on` buttonActivated $ do
        newdata <- sequence [ getAllText line | line <- lines ]
        COM.insert inj newdata
        widgetDestroy win
        mainQuit
      boxPackStart controlBox collectButton PackNatural 1
    Show -> do
      sequence_ [ line `set` [textViewEditable := False] | line <- lines ]
      unlockButton  <- buttonNewWithLabel "unlock"
      unlockButton `on` buttonActivated $ do
        widgetDestroy win
        collect inj prefills Update (wdth, ht)
      deleteButton  <- buttonNewWithLabel "delete"
      deleteButton `on` buttonActivated $ do
        widgetDestroy win
        collect inj prefills Delete (wdth, ht)
      boxPackStart controlBox unlockButton PackNatural 1
      boxPackStart controlBox deleteButton PackNatural 1
    Update -> do
      updateButton <- buttonNewWithLabel "update"
      updateButton `on` buttonActivated $ do
        COM.delete inj prefills
        newdata <- sequence [ getAllText line | line <- lines ]
        COM.insert inj newdata
        widgetDestroy win
        mainQuit
      boxPackStart controlBox updateButton PackNatural 1
    Delete -> do
      sequence_ [ line `set` [textViewEditable := False] | line <- lines ]
      lbl <- labelNew $ Just "really want to delete?"
      yesButton <- buttonNewWithLabel "yes"
      yesButton `on` buttonActivated $ do
        COM.delete inj prefills
        widgetDestroy win
        mainQuit
      noButton <- buttonNewWithLabel "no"
      noButton `on` buttonActivated $ widgetDestroy win
      boxPackStart controlBox lbl PackNatural 1
      boxPackStart controlBox yesButton PackNatural 1
      boxPackStart controlBox noButton PackNatural 1
  boxPackStart vb gr PackNatural 1
  boxPackStart vb controlBox PackNatural 1
  containerAdd win vb
  widgetShowAll win
  windowResize win 100 10
  case role of
    Show -> return ()
    _    -> mainGUI

xseli :: String -> IO ()
xseli x = do
  (Just hin, _, _, _) <- createProcess (proc "xsel" [ "-i" ]){ std_in = CreatePipe }
  hPutStr hin x >> hClose hin

makeTable :: COM.Injection -> [[String]] -> (Width, Height) -> IO Table
makeTable inj xss (wdth, ht) = do
  let ttls = map fst $ COM.rowspecs inj
  tbl <- tableNew (1 + length xss) (2 + length ttls) False
  sequence_ [
    do
      btn <- buttonNewWithLabel (ttls !! i)
      lbl <- binGetChild btn
      (fromJust lbl) `set` [widgetHExpand  := True]
      addClass (toWidget btn) $ "linii-button-colname"
      addClass (fromJust lbl) $ "linii-label-colname"
      addClass (fromJust lbl) $ "linii-label-col-" ++ (ttls !! i)
      tableAttachDefaults tbl btn (i + 1) (i + 2) 0 1
    | i <- [0..(length ttls - 1)]
    ]
  sequence_ [
    do
      ebtn <- buttonNewWithLabel "☞"
      elbl <- binGetChild ebtn
      addClass (fromJust elbl) "show-button"
      ebtn `on` buttonActivated $ collect inj (xss !! j) Show (wdth, ht)
      tableAttachDefaults tbl ebtn 0 1 (j + 1) (j + 2)
      sequence_ [
        do
          btn <- buttonNewWithLabel $ GUI.truncate ((xss !! j) !! i) wtt
          btn `on` buttonActivated $ xseli ((xss !! j) !! i)
          (toWidget btn) `set` [widgetHasTooltip := True]
          widgetSetTooltipText btn $ Just ((xss !! j) !! i)
          lbl <- binGetChild btn
          widgetSetVAlign (fromJust lbl) AlignFill
          widgetSetHAlign (fromJust lbl) AlignFill
          (fromJust lbl) `set` [widgetHExpand  := True]
          addClass (toWidget btn) $ "linii-button-value"
          addClass (fromJust lbl) $ "linii-label-value"
          addClass (fromJust lbl) $ "linii-label-col-" ++ (ttls !! i)
          tableAttachDefaults tbl btn (i + 1) (i + 2) (j + 1) (j + 2)
        | (i, (cttl, (wtt, _))) <- zip [0..(length ttls - 1)] $ COM.rowspecs inj
        ]
    | j <- [0..(length xss - 1)]
    ]
  return tbl

type Query      = String
type BottomVBox = VBox
type GEnv = (Window, Width, Height, BottomVBox)

insertRowBox :: GEnv -> COM.Injection -> [[String]] -> Query -> Int -> IO ()
insertRowBox genv inj xss query startIndex = do
  rowBox <- vBoxNew False 2
  let (mainwin, wdth, ht, bottomVBox) = genv
  batchLabel <- labelNew $ Just (show ((+) 1 $ startIndex `quot` rowsetBatchSize) ++
                                 "/" ++
                                 show ((+) 1 $ length xss  `quot` rowsetBatchSize))
  tbl <- makeTable inj (take rowsetBatchSize (drop startIndex xss)) (wdth, ht)
  tableButtonsHBox <- hBoxNew False 2
  addClass (toWidget tableButtonsHBox) "results-top-hbox"
  goPrevBatchButton <- buttonNewWithLabel (if startIndex == 0 then "--" else "⇦")
  goPrevBatchButton `on` buttonActivated $ do
    if startIndex == 0
      then return ()
      else do
      widgetDestroy rowBox
      insertRowBox genv inj xss query $ let newInd = startIndex - rowsetBatchSize
                                        in if newInd > 0
                                           then newInd
                                           else 0
  goNextBatchButton <- buttonNewWithLabel $ if startIndex >= length xss - rowsetBatchSize
                                            then "--"
                                            else "⇨"
  goNextBatchButton `on` buttonActivated $ do
    if startIndex < (length xss - rowsetBatchSize)
      then do
      widgetDestroy rowBox
      insertRowBox genv inj xss query (startIndex + rowsetBatchSize)
      else return ()
  newButton <- buttonNewWithLabel "new"
  newButton `on` buttonActivated $ collect inj [ "" | _ <- COM.rowspecs inj ] Collect (wdth, ht)
  refreshButton <- buttonNewWithLabel "refresh"
  refreshButton `on` buttonActivated $ do
    widgetDestroy rowBox
    newlines <- if query == "" then COM.selectAll inj else COM.select inj query
    insertRowBox genv inj newlines query startIndex
  delButton <- buttonNewWithLabel "del"
  delButton `on` buttonActivated $ widgetDestroy rowBox >> windowResize mainwin wdth ht
  queryLabel <- labelNew $ if query == "" then Nothing else Just (GUI.truncate query 50)
  boxPackStart tableButtonsHBox goPrevBatchButton PackNatural 1
  boxPackStart tableButtonsHBox goNextBatchButton PackNatural 1
  boxPackStart tableButtonsHBox newButton PackNatural 1
  boxPackStart tableButtonsHBox refreshButton PackNatural 1
  boxPackStart tableButtonsHBox delButton PackNatural 1
  boxPackStart tableButtonsHBox queryLabel PackNatural 1
  boxPackEnd   tableButtonsHBox batchLabel PackNatural 1
  boxPackStart rowBox tableButtonsHBox PackNatural 1
  boxPackEnd rowBox tbl PackNatural 1
  boxPackStart bottomVBox rowBox PackNatural 1
  widgetShowAll rowBox
  windowResize mainwin wdth 10
  
starter :: COM.Injection -> IO ()
starter inj = do
  wh <- newEmptyMVar
  _ <- initGUI
  mainwin <- windowNew
  mainwin `set` [windowTitle := COM.dbpath inj]
  mainwin `on` deleteEvent $ liftIO mainQuit >> return False
  masterVBox <- vBoxNew False 2
  addClass (toWidget masterVBox) "commander-main"
  containerAdd mainwin masterVBox
  topVBox <- vBoxNew False 2
  bottomVBox <- vBoxNew False 2
  addClass (toWidget topVBox) "commander-top"
  cmdline <- textViewNew
  addClass (toWidget cmdline) "line-entry"
  cmdline `on` keyPressEvent $ tryEvent $ do
    mods    <- eventModifier
    keyname <- T.unpack <$> eventKeyName
    case (mods, keyname) of
      ([Control], "Return") -> liftIO $ do
        q <- getAllText cmdline
        (w, h)  <- readMVar wh
        results <- if q == "" then COM.selectAll inj else COM.select inj q
        insertRowBox (mainwin, w, h, bottomVBox) inj results q 0 
  ruler <- labelNew $ Just ['-'| _ <- [1..100]]
  addClass (toWidget ruler) "line-entry"
  boxPackStart topVBox cmdline PackNatural 1
  boxPackStart masterVBox topVBox PackNatural 1
  boxPackEnd masterVBox bottomVBox PackNatural 1
  boxPackStart topVBox ruler PackNatural 1
  widgetShowAll mainwin
  Requisition width100 height1 <- widgetSizeRequest ruler
  putMVar wh (width100, height1)
  widgetDestroy ruler
  windowResize mainwin width100 10
  screen <- screenGetDefault
  styleProvider <- cssProviderNew
  cssProviderLoadFromPath styleProvider (COM.csspath inj)
  styleContextAddProviderForScreen (fromJust screen) styleProvider 799
  mainGUI
