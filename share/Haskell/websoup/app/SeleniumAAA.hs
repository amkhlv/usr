{-# LANGUAGE OverloadedStrings #-}

module SeleniumAAA where
    
import Lib
import Data.Text
import Test.WebDriver 
import Test.WebDriver.Session
import Test.WebDriver.Commands
import Test.WebDriver.JSON (ignoreReturn)
import qualified Control.Monad.IO.Class as IOC
import System.Directory

continue = askWhatNext things
things :: Things
things = [
    ( "a", 
      "navigate to...", 
      continue
      )
  , ( "q",
      "quit",
      return ()
      )
  ]
googleIt :: WD ()
googleIt = do
  openPage "https://google.com"
  continue
  closeSession

main :: IO ()
main = do
  homeDir <- getHomeDirectory
  getPassword "example" "example@gmail.com" >>= putStrLn
  ffConfig <- getFFConfig $ homeDir ++ "/.config/amkhlv/websoup/selenium.json"
  runSession ffConfig googleIt
