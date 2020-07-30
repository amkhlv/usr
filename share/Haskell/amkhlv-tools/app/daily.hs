{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Main where

import Lib
import Control.Monad.IO.Class
import Data.Text (Text, pack, unpack)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import Data.Maybe
import Data.ByteString (ByteString)
import Control.Applicative
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Layout.HBox
import Graphics.UI.Gtk.Layout.VBox
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.General.CssProvider
import Graphics.UI.Gtk.General.StyleContext
import Graphics.UI.Gtk.Gdk.Screen
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Misc.Tooltip
import System.Process
import System.Directory
import System.FilePath.Posix

data Task = Task {
  description :: Text
  , script :: Text
  } deriving (Eq, Show)

instance FromJSON Task where
  parseJSON (Y.Object v) =
    Task <$> v .: "description" <*> v .: "script"
  parseJSON _ = fail "Expected Task"


configDir :: IO (FilePath)
configDir = (\p -> p </> (".config/amkhlv/daily/" :: FilePath)) <$> getHomeDirectory 

runScript :: Text -> IO ()
runScript s = do
  confDir <- configDir
  _ <- createProcess $ shell $ "sh -c " ++ (confDir </> "bin" </> unpack s)
  return ()

catScript :: Text -> IO (String)
catScript s = do
  confDir <- configDir
  readFile (confDir </> "bin" </> unpack s)

selfEdit :: IO ()
selfEdit = do
  confDir <- configDir
  _ <- createProcess $ proc "glade" [ confDir </> "ui.glade"]
  return ()

yamlEdit :: IO ()
yamlEdit = do
  confDir <- configDir
  _ <- createProcess $ proc "nvim-qt" [confDir </> "list.yaml"]
  return ()

addClass :: Widget -> String -> IO ()
addClass w name = do
  styleCtxt <- widgetGetStyleContext w
  styleContextAddClass styleCtxt name  

removeClass :: Widget -> String -> IO ()
removeClass w name = do
  styleCtxt <- widgetGetStyleContext w
  styleContextRemoveClass styleCtxt name

main :: IO ()
main = do
  confDir <- configDir
  tasksE <- Y.decodeFileEither $ confDir </> "list.yaml"
  case tasksE of
    Left e -> putStrLn "could not decode"
    Right y -> do
      print (y :: [Task])
      _ <- initGUI
      provider <- cssProviderNew
      cssProviderLoadFromPath provider $ confDir </> "style.css"
      builder <- builderNew
      builderAddFromFile builder $ confDir </> "ui.glade"
      window <- builderGetObject builder castToWindow ("mainWin" :: String)
      window `on` deleteEvent $ liftIO mainQuit >> return False
      screen <- screenGetDefault 
      styleContextAddProviderForScreen (fromJust screen) provider 799
      vboxLeft <- builderGetObject builder castToBox ("leftVBox" :: String)
      sequence_ [ do
                    b <- buttonNew
                    l <- labelNew (Just $ description t)
                    cat <- catScript $ script t
                    widgetSetTooltipText b $ Just cat
                    addClass (toWidget l) "daily-item-label-pending"
                    containerAdd b l
                    addClass (toWidget b) "daily-item-pending"
                    b `on` buttonActivated $ do
                      removeClass (toWidget b) "daily-item-pending"
                      removeClass (toWidget l) "daily-item-label-pending"
                      addClass (toWidget b) "daily-item-visited"
                      addClass (toWidget l) "daily-item-label-visited"
                      liftIO (runScript $ script t )
                    containerAdd vboxLeft b
                | t <- y
                ]
      vboxRight <- builderGetObject builder castToBox ("rightVBox" :: String)
      editBtn <- builderGetObject builder castToButton ("editButton" :: String)
      editBtn `on` buttonActivated $ liftIO selfEdit
      yamlBtn <- builderGetObject builder castToButton ("yamlButton" :: String)
      yamlBtn `on` buttonActivated $ liftIO yamlEdit
      restartBtn <- builderGetObject builder castToButton ("restartButton" :: String)
      restartBtn `on` buttonActivated $ do
        widgetDestroy window
        liftIO mainQuit
        liftIO main
      widgetShowAll window
      mainGUI
      

