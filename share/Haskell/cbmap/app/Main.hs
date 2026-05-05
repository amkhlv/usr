module Main where

import Control.Exception (IOException, displayException, try)
import Control.Monad (unless, void)
import Data.GI.Base
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import System.Exit (ExitCode (..))
import System.IO (hClose)
import System.Process
  ( CreateProcess (std_in),
    StdStream (CreatePipe),
    createProcess,
    proc,
    readCreateProcessWithExitCode,
    shell,
    waitForProcess,
  )

main :: IO ()
main = do
  app <- new Gtk.Application [#applicationId := ("local.amkhlv.cbmap" :: Text.Text)]
  void $ on app #activate (buildUi app)
  void $ Gio.applicationRun app Nothing

buildUi :: Gtk.Application -> IO ()
buildUi app = do
  initialText <- readCommandOutput "wl-paste" ["--no-newline"] ""
  previousTextRef <- newIORef Nothing

  buffer <- Gtk.textBufferNew (Nothing :: Maybe Gtk.TextTagTable)
  Gtk.textBufferSetText buffer initialText (-1)

  textView <-
    new
      Gtk.TextView
      [ #buffer := buffer,
        #editable := True,
        #monospace := True,
        #vexpand := True,
        #hexpand := True,
        #wrapMode := Gtk.WrapModeWordChar
      ]

  scrolledWindow <-
    new
      Gtk.ScrolledWindow
      [ #child := textView,
        #vexpand := True,
        #hexpand := True
      ]
  Gtk.scrolledWindowSetPolicy
    scrolledWindow
    Gtk.PolicyTypeAutomatic
    Gtk.PolicyTypeAutomatic

  commandEntry <-
    new
      Gtk.Entry
      [ #placeholderText := ("Shell command" :: Text.Text),
        #hexpand := True
      ]

  undoButton <- new Gtk.Button [#label := ("undo" :: Text.Text)]
  copyButton <- new Gtk.Button [#label := ("copy" :: Text.Text)]

  buttonBox <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationHorizontal,
        #spacing := 8
      ]
  Gtk.boxAppend buttonBox undoButton
  Gtk.boxAppend buttonBox copyButton

  mainBox <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationVertical,
        #spacing := 8,
        #marginTop := 8,
        #marginBottom := 8,
        #marginStart := 8,
        #marginEnd := 8
      ]
  Gtk.boxAppend mainBox scrolledWindow
  Gtk.boxAppend mainBox commandEntry
  Gtk.boxAppend mainBox buttonBox

  window <-
    new
      Gtk.ApplicationWindow
      [ #application := app,
        #title := ("cbmap" :: Text.Text),
        #defaultWidth := 900,
        #defaultHeight := 700,
        #child := mainBox
      ]

  void $ on commandEntry #activate $ do
    command <- Gtk.editableGetText commandEntry
    Gtk.editableSetText commandEntry ("" :: Text.Text)
    unless (Text.all (`elem` [' ', '\t']) command || Text.null command) $ do
      currentText <- getBufferText buffer
      writeIORef previousTextRef (Just currentText)
      output <- runShellFilter command currentText
      Gtk.textBufferSetText buffer output (-1)

  void $ on undoButton #clicked $
    restorePrevious buffer previousTextRef

  void $ on copyButton #clicked $ do
    currentText <- getBufferText buffer
    writeCommandInput "wl-copy" [] currentText
    Gio.applicationQuit app

  Gtk.windowPresent window
  void $ Gtk.widgetGrabFocus commandEntry

getBufferText :: Gtk.TextBuffer -> IO Text.Text
getBufferText buffer = do
  (startIter, endIter) <- Gtk.textBufferGetBounds buffer
  Gtk.textBufferGetText buffer startIter endIter True

restorePrevious :: Gtk.TextBuffer -> IORef (Maybe Text.Text) -> IO ()
restorePrevious buffer previousTextRef = do
  previousText <- readIORef previousTextRef
  case previousText of
    Nothing -> pure ()
    Just text -> Gtk.textBufferSetText buffer text (-1)

runShellFilter :: Text.Text -> Text.Text -> IO Text.Text
runShellFilter command input = do
  result <-
    try (readCreateProcessWithExitCode (shell (Text.unpack command)) (Text.unpack input)) ::
      IO (Either IOException (ExitCode, String, String))
  pure $
    case result of
      Left err -> Text.pack (displayException err)
      Right (ExitSuccess, stdoutText, _) -> Text.pack stdoutText
      Right (ExitFailure _, stdoutText, stderrText)
        | null stdoutText -> Text.pack stderrText
        | otherwise -> Text.pack stdoutText

readCommandOutput :: FilePath -> [String] -> Text.Text -> IO Text.Text
readCommandOutput command args fallback = do
  result <-
    try (readCreateProcessWithExitCode (proc command args) "") ::
      IO (Either IOException (ExitCode, String, String))
  pure $
    case result of
      Left _ -> fallback
      Right (ExitSuccess, stdoutText, _) -> Text.pack stdoutText
      Right _ -> fallback

writeCommandInput :: FilePath -> [String] -> Text.Text -> IO ()
writeCommandInput command args input = do
  result <- try go :: IO (Either IOException ())
  case result of
    Left _ -> pure ()
    Right () -> pure ()
  where
    go = do
      (stdinHandle, _, _, processHandle) <-
        createProcess (proc command args) {std_in = CreatePipe}
      case stdinHandle of
        Nothing -> pure ()
        Just handle -> do
          TextIO.hPutStr handle input
          hClose handle
          void $ waitForProcess processHandle
