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
import qualified Data.Map as Map
import Data.Text (pack, unpack)
import qualified GI.GLib as GLib
import qualified GI.Gdk.Objects.Display as GD
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

mkrow :: Gtk.Application -> Gtk.ApplicationWindow -> JSObject JSValue -> Int32 -> IORef (Map.Map String Action) -> IO Gtk.Box
mkrow app window obj rowNum hints = do
  box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #cssClasses := ["amkhlv-data-main-box"]]
  colNum <- newIORef 0
  sequence_
    [ do
        n <- readIORef colNum
        let charhint = [intToChar $ fromIntegral rowNum, intToChar n]
        hintLabel <- new Gtk.Label [#label := pack $ [' '] ++ charhint ++ [':'], #cssClasses := ["amkhlv-data-hint-label"]]
        Gtk.boxAppend box hintLabel
        button <-
          new
            Gtk.Button
            [ #label := pack k,
              #cssClasses := case v of
                JSString _ -> ["amkhlv-data-button"]
                JSObject _ -> ["amkhlv-data-expand-button"]
                _ -> ["amkhlv-data-button"],
              On #clicked $ case v of
                JSString s -> do
                  wlCopy s
                  #quit app
                JSObject obj' -> do
                  view <- mkview app window [obj']
                  Gtk.windowSetChild window (Just view)
                _ -> pure ()
            ]
        Gtk.boxAppend box button
        modifyIORef hints $
          case v of
            JSString s -> Map.insert charhint (CopyAction s)
            JSObject obj' -> Map.insert charhint (ExpandAction obj')
            _ -> (\x -> x)
        modifyIORef colNum (+ 1)
    | (k, v) <- fromJSObject obj
    ]
  return box

mkview :: Gtk.Application -> Gtk.ApplicationWindow -> [JSObject JSValue] -> IO Gtk.Box
mkview app window stack =
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
                  row <- mkrow app window obj n hintMap
                  Gtk.gridAttach grid row 1 n 1 1
                JSString str -> do
                  charhint <- (intToChar . fromIntegral) <$> readIORef rowNum
                  hbox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #cssClasses := ["amkhlv-data-row-hbox"]]
                  label1 <- new Gtk.Label [#label := pack [' ', ' ', charhint, ':'], #cssClasses := ["amkhlv-data-hint-label"]]
                  modifyIORef hintMap (Map.insert [charhint] (CopyAction str))
                  button <-
                    new
                      Gtk.Button
                      [ #label := "📋",
                        #cssClasses := ["amkhlv-copy-button"],
                        On #clicked $ do
                          wlCopy str
                          #quit app
                      ]
                  Gtk.boxAppend hbox label1
                  Gtk.boxAppend hbox button
                  Gtk.gridAttach grid hbox 1 n 1 1
                _ -> pure ()
              modifyIORef rowNum (+ 1)
          | (k, v) <- fromJSObject top
          ]
        Gtk.boxAppend box grid
        entry <-
          new
            Gtk.Entry
            [#placeholderText := "xx", #cssClasses := ["amkhlv-data-line-entry"]]
        _ <- on entry #activate $ do
          str <- unpack <$> get entry #text
          hints <- readIORef hintMap
          case Map.lookup str hints of
            Just (CopyAction s) -> do
              wlCopy s
              #quit app
            Just (ExpandAction obj') -> do
              view <- mkview app window $ obj' : stack
              Gtk.windowSetChild window (Just view)
            Nothing -> case stack of
              [] -> #quit app
              _ : xs -> do
                view <- mkview app window xs
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
    view <- mkview app window [obj]
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
          print rootObj
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
