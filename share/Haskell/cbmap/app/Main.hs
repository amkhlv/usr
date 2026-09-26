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
  scrcpyButton <- new Gtk.Button [#label := ("scrcpy" :: Text.Text)]
  latex2svgButton <- new Gtk.Button [#label := ("LaTeX2SVG" :: Text.Text)]

  buttonBox <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationHorizontal,
        #spacing := 8
      ]
  Gtk.boxAppend buttonBox undoButton
  Gtk.boxAppend buttonBox copyButton
  Gtk.boxAppend buttonBox scrcpyButton
  Gtk.boxAppend buttonBox latex2svgButton

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

  void $ on scrcpyButton #clicked $ do
    currentText <- getBufferText buffer
    void $
      readCommandOutput
        "adb"
        ["shell", Text.unpack (adbInputCommand currentText)]
        ""

  void $ on latex2svgButton #clicked $ do
    formula <- getBufferText buffer
    conversionResult <- convertLatexFormulaToSvg formula
    case conversionResult of
      Left errorText -> Gtk.textBufferSetText buffer errorText (-1)
      Right svgText -> do
        writeCommandInput "wl-copy" ["--type", "image/x-inkscape-svg"] svgText
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

shellQuote :: Text.Text -> Text.Text
shellQuote text = "'" <> Text.replace "'" "'\\''" text <> "'"

adbInputCommand :: Text.Text -> Text.Text
adbInputCommand text =
  Text.intercalate " && " (map inputTextCommand arguments)
  where
    parts = Text.splitOn "%s" text
    arguments =
      case filter (not . Text.null) splitArguments of
        [] -> [""]
        nonEmptyArguments -> nonEmptyArguments
    splitArguments =
      case parts of
        [] -> []
        firstPart : remainingParts ->
          firstPart
            : concatMap
              (\part -> ["%", "s", part])
              remainingParts
    inputTextCommand value = "input text " <> shellQuote value

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

convertLatexFormulaToSvg :: Text.Text -> IO (Either Text.Text Text.Text)
convertLatexFormulaToSvg formula = do
  tempDirResult <- runToolCapturingOutput "mktemp" ["-d"]
  case tempDirResult of
    Left err -> pure (Left err)
    Right tempDirOutput -> do
      let tempDir = Text.unpack (Text.strip tempDirOutput)
      result <- compileFormulaInDir tempDir formula
      void $ readCommandOutput "rm" ["-rf", tempDir] ""
      pure result

compileFormulaInDir :: FilePath -> Text.Text -> IO (Either Text.Text Text.Text)
compileFormulaInDir tempDir formula = do
  let texPath = tempDir <> "/formula.tex"
      dviPath = tempDir <> "/formula.dvi"
      svgPath = tempDir <> "/formula.svg"
  TextIO.writeFile texPath (latexSource formula)
  latexResult <-
    runToolExpectingSuccess
      "latex"
      ["-interaction=nonstopmode", "-halt-on-error", "-output-directory=" <> tempDir, texPath]
  case latexResult of
    Left err -> pure (Left err)
    Right () -> do
      dvisvgmResult <- runToolExpectingSuccess "dvisvgm" ["--no-fonts", "-o", svgPath, dviPath]
      case dvisvgmResult of
        Left err -> pure (Left err)
        Right () -> do
          svgReadResult <- try (TextIO.readFile svgPath) :: IO (Either IOException Text.Text)
          pure $ case svgReadResult of
            Left err -> Left (Text.pack (displayException err))
            Right svgText -> Right svgText

latexSource :: Text.Text -> Text.Text
latexSource formula =
  Text.unlines
    [ "\\documentclass{article}",
      "\\usepackage{amsmath}",
      "\\usepackage{amssymb}",
      "\\pagestyle{empty}",
      "\\begin{document}",
      "\\[ " <> formula <> " \\]",
      "\\end{document}"
    ]

runToolExpectingSuccess :: FilePath -> [String] -> IO (Either Text.Text ())
runToolExpectingSuccess command args = do
  outcome <- runToolCapturingOutput command args
  pure $ case outcome of
    Left err -> Left err
    Right _ -> Right ()

runToolCapturingOutput :: FilePath -> [String] -> IO (Either Text.Text Text.Text)
runToolCapturingOutput command args = do
  result <-
    try (readCreateProcessWithExitCode (proc command args) "") ::
      IO (Either IOException (ExitCode, String, String))
  pure $ case result of
    Left err -> Left (Text.pack (displayException err))
    Right (ExitSuccess, stdoutText, _) -> Right (Text.pack stdoutText)
    Right (ExitFailure _, stdoutText, stderrText) ->
      Left (Text.pack (if null stderrText then stdoutText else stderrText))

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
