{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (bracket, throwIO)
import Control.Monad (void)
import Data.Char (chr, ord)
import Data.GI.Base
import Data.IORef
import Data.Int (Int32)
import Data.List (intersperse)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (pack, unpack)
import qualified GI.GLib as GLib
import qualified GI.Gdk.Objects.Display as GD
import qualified GI.Gio as Gio
import qualified GI.Gio.Interfaces.File as GFile
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Constants as GtkConst
import qualified GI.Gtk.Objects.CssProvider as GtkProvider
import qualified GI.Gtk.Objects.StyleContext as GtkStyleContext
import System.Directory (doesFileExist, getHomeDirectory, makeAbsolute)
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hPutStr)
import System.Process
import Text.JSON

intToChar :: Int -> Char
intToChar n = chr (ord 'a' + n)

data Key = RowKey String | BtnKey String

stripColon :: String -> String
stripColon = reverse . stripColon' . reverse

stripColon' [] = []
stripColon' (':' : rest) = rest
stripColon' rest = rest

mkClassSuffix :: [Key] -> String
mkClassSuffix = mkClassSuffix' . reverse

mkClassSuffix' [] = ""
mkClassSuffix' (RowKey k : ks) = "--" ++ k ++ mkClassSuffix' ks
mkClassSuffix' (BtnKey k : ks) = "-" ++ stripColon k ++ mkClassSuffix' ks

data Action = CopyAction JSString | ExpandAction (JSObject JSValue)

wlCopy :: JSString -> IO ()
wlCopy txt = do
  withCreateProcess ((proc "wl-copy" []) {std_in = CreatePipe}) $ \mHin _ _ ph -> do
    hin <- maybe (throwIO (userError "wl-copy: no stdin pipe")) pure mHin
    hPutStr hin (fromJSString txt)
    hClose hin
    ec <- waitForProcess ph
    case ec of
      ExitSuccess -> pure ()
      ExitFailure c -> throwIO (userError ("wl-copy failed with exit code " ++ show c))

mkrow :: Gtk.Application -> Gtk.ApplicationWindow -> JSObject JSValue -> Int32 -> IORef (Map.Map String (Action, Gtk.Button, String, Maybe String)) -> [Key] -> [JSObject JSValue] -> IO Gtk.Box
mkrow app window obj rowNum hints keys stack = do
  box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #cssClasses := ["amkhlv-data-row", pack $ "amkhlv-data-row" ++ mkClassSuffix keys]]
  putStrLn $ "amkhlv-data-row" ++ mkClassSuffix keys
  colNum <- newIORef 0
  sequence_
    [ do
        n <- readIORef colNum
        let charhint = [intToChar $ fromIntegral rowNum, intToChar n]
        hintLabel <- new Gtk.Label [#label := pack $ charhint, #cssClasses := ["amkhlv-data-hint-label"]]
        Gtk.boxAppend box hintLabel
        button <-
          new
            Gtk.Button
            [ #label := case v of
                JSString w -> case reverse k of
                  ':' : _ -> pack $ fromJSString w
                  _ -> pack k
                JSObject _ -> pack k
                _ -> pack $ "Unknown " ++ k,
              #cssClasses := case v of
                JSString _ -> ["amkhlv-data-button", pack $ "amkhlv-data-button" ++ (mkClassSuffix $ BtnKey k : keys)]
                JSObject _ -> ["amkhlv-data-expand-button", pack $ "amkhlv-data-expand-button" ++ (mkClassSuffix $ BtnKey k : keys)]
                _ -> ["amkhlv-data-unknown-type-button"],
              On #clicked $ case v of
                JSString s -> do
                  wlCopy s
                  #quit app
                JSObject obj' -> do
                  view <- mkview app window (BtnKey k : keys) $ obj' : stack
                  Gtk.windowSetChild window (Just view)
                _ -> pure ()
            ]
        putStrLn $ case v of
          JSString _ -> "amkhlv-data-button" ++ (mkClassSuffix $ BtnKey k : keys)
          JSObject _ -> "amkhlv-data-expand-button" ++ (mkClassSuffix $ BtnKey k : keys)
          _ -> "amkhlv-data-unknown-type-button"
        Gtk.boxAppend box button
        let rowKey = case keys of (RowKey k : _) -> k; _ -> "_ERROR_"
        modifyIORef hints $
          case v of
            JSString s -> Map.insert charhint ((CopyAction s), button, rowKey, Just k)
            JSObject obj' -> Map.insert charhint ((ExpandAction obj'), button, rowKey, Just k)
            _ -> (\x -> x)
        modifyIORef colNum (+ 1)
    | (k, v) <- fromJSObject obj
    ]
  return box

mkview :: Gtk.Application -> Gtk.ApplicationWindow -> [Key] -> [JSObject JSValue] -> IO Gtk.Box
mkview app window keys stack =
  let top = head stack
   in do
        hintMap <- newIORef Map.empty
        box <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["amkhlv-data-view-box"]]
        grid <- new Gtk.Grid [#orientation := Gtk.OrientationHorizontal, #cssClasses := ["amkhlv-data-view-grid"]]
        rowNum <- newIORef 0
        sequence_
          [ do
              label <- new Gtk.Label [#label := pack k, #cssClasses := ["amkhlv-data-row-label"]]
              n <- readIORef rowNum
              Gtk.gridAttach grid label 0 n 1 1
              case v of
                JSObject obj -> do
                  row <- mkrow app window obj n hintMap (RowKey k : keys) stack
                  Gtk.gridAttach grid row 1 n 1 1
                JSString str -> do
                  charhint <- (intToChar . fromIntegral) <$> readIORef rowNum
                  hbox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #cssClasses := ["amkhlv-data-row-singleitem"]]
                  label1 <- new Gtk.Label [#label := pack [charhint, charhint], #cssClasses := ["amkhlv-data-hint-label"]]
                  button <-
                    new
                      Gtk.Button
                      [ #label := "📋",
                        #cssClasses := ["amkhlv-copy-button", pack $ "amkhlv-copy-button" ++ (mkClassSuffix $ RowKey k : keys)],
                        On #clicked $ do
                          wlCopy str
                          #quit app
                      ]
                  putStrLn $ "amkhlv-copy-button" ++ (mkClassSuffix $ RowKey k : keys)
                  modifyIORef hintMap (Map.insert [charhint, charhint] ((CopyAction str), button, k, Nothing))
                  Gtk.boxAppend hbox label1
                  Gtk.boxAppend hbox button
                  Gtk.gridAttach grid hbox 1 n 1 1
                _ -> pure ()
              modifyIORef rowNum (+ 1)
          | (k, v) <- fromJSObject $ top
          ]
        Gtk.boxAppend box grid
        entry <-
          new
            Gtk.Entry
            [#placeholderText := "xx", #cssClasses := ["amkhlv-data-line-entry"], #halign := Gtk.AlignCenter, #widthChars := 2, #maxWidthChars := 2]
        _ <- on entry #changed $ do
          str <- unpack <$> get entry #text
          if length str < 2
            then pure ()
            else do
              hints <- readIORef hintMap
              case Map.lookup str hints of
                Just ((CopyAction s), button, _, _) -> do
                  wlCopy s
                  Gtk.widgetSetCssClasses button ["amkhlv-data-selected-button"]
                  _ <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 250 $ do
                    #quit app
                    pure GLib.SOURCE_REMOVE
                  pure ()
                Just ((ExpandAction obj'), _, rowKey, Just key) -> do
                  view <- mkview app window (BtnKey key : RowKey rowKey : keys) (obj' : stack)
                  Gtk.windowSetChild window (Just view)
                Just ((ExpandAction obj'), _, rowKey, Nothing) -> error "Invalid row key"
                Nothing -> pure ()
        _ <- on entry #activate $ do
          str <- unpack <$> get entry #text
          hints <- readIORef hintMap
          case Map.lookup str hints of
            Just ((CopyAction s), button, _, _) -> do
              -- this should never happen though, because would be caught in #changed
              Gtk.widgetSetCssClasses button ["amkhlv-data-selected-button"]
              wlCopy s
              #quit app
            Just ((ExpandAction obj'), _, _, Just key) -> do
              -- this should never happen though
              view <- mkview app window (BtnKey key : keys) (obj' : stack)
              Gtk.windowSetChild window (Just view)
            Just ((ExpandAction obj'), _, rowKey, Nothing) -> error "Invalid row key"
            Nothing -> case stack of
              [] -> #quit app
              [_] -> #quit app
              _ : xs -> do
                let ks = case keys of [] -> []; [_] -> []; _ : _ : rest -> rest
                view <- mkview app window ks xs
                Gtk.windowSetChild window (Just view)

        Gtk.boxAppend box entry
        _ <- GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
          _ <- Gtk.widgetGrabFocus entry
          pure GLib.SOURCE_REMOVE

        return box

activate :: Gtk.Application -> JSValue -> IO ()
activate app rootObj = case rootObj of
  JSObject obj -> do
    mdisplay <- GD.displayGetDefault
    provider <- new GtkProvider.CssProvider []
    home <- getHomeDirectory
    cssFile <- GFile.fileNewForPath $ home ++ "/.config/amkhlv/littledata.css"
    Gtk.cssProviderLoadFromFile provider cssFile
    mapM_ (\disp -> GtkStyleContext.styleContextAddProviderForDisplay disp provider $ fromIntegral GtkConst.STYLE_PROVIDER_PRIORITY_USER - 1) mdisplay

    window <-
      new
        Gtk.ApplicationWindow
        [ #application := app,
          #cssClasses :=
            ["amkhlv-data-main-window"],
          #title :=
            "Little Data"
        ]
    -- Action: app.quit
    quitAction <- Gio.simpleActionNew "quit" Nothing
    _ <- on quitAction #activate $ \_ -> #quit app
    Gio.actionMapAddAction app quitAction
    -- Keybinding: Escape -> app.quit
    Gtk.applicationSetAccelsForAction app "app.quit" ["Escape"]

    view <- mkview app window [] [obj]
    Gtk.windowSetChild window (Just view)
    window.show
  _ -> return ()

main :: IO ()
main = do
  home <- getHomeDirectory
  (exitCode, out, err) <-
    readProcessWithExitCode
      "dhall-to-json"
      ["--file", home ++ "/.config/amkhlv/littledata.dhall"]
      ""

  case exitCode of
    ExitFailure code -> do
      putStrLn $ "Process failed with exit code " ++ show code
      putStrLn err
    ExitSuccess ->
      case decode out :: Result JSValue of
        Ok rootObj -> do
          putStrLn "Parsed successfully."
          app <-
            new
              Gtk.Application
              [ #applicationId := "amkhlv-data",
                On #activate (activate ?self rootObj)
              ]

          void $ app.run Nothing
        Error e -> do
          putStrLn $ "Invalid JSON from dhall-to-json:"
          putStrLn e
          putStrLn "\nTEXT:\n"
          putStrLn out
