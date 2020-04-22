{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Lib (
    getPassword
    , getSiteAndAccount
    , getFFConfig
    , Things
    , module AmkhlvSecrets
    , askWhatNext
    ) where

import AmkhlvSecrets
import Data.Text
import Data.Aeson
import GHC.Generics
import System.Directory
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Maybe
import Test.WebDriver 
import Test.WebDriver.Firefox.Profile
import qualified Control.Monad.IO.Class as IOC
import System.Console.ANSI

data ConfSecrets = ConfSecrets {
    secretFile :: String
} deriving (Generic, Show)
instance ToJSON ConfSecrets where toEncoding = genericToEncoding defaultOptions
instance FromJSON ConfSecrets

data ConfSelenium = ConfSelenium {
  driverServerHost :: String ,
  driverServerPort :: Int ,
  driverServerHTTPRetryCount :: Int ,
  driverFFProfile :: Maybe String
} deriving (Generic, Show)
instance ToJSON ConfSelenium where toEncoding = genericToEncoding defaultOptions
instance FromJSON ConfSelenium

getFFConfig :: FilePath -> IO WDConfig
getFFConfig fp = do 
  confStr <- readFile fp 
  let conf = fromJust (( decode $ C8.pack confStr ) :: Maybe ConfSelenium)
  prof <- sequence $ prepareLoadedProfile_ <$> driverFFProfile conf 
  let ff = Firefox {
    ffProfile = prof, ffLogPref = LogInfo, ffBinary = Nothing, ffAcceptInsecureCerts = Just False
    }
  return $ useBrowser ff defaultConfig { 
    wdHost = driverServerHost conf, 
    wdPort = driverServerPort conf, 
    wdHTTPRetryCount = driverServerHTTPRetryCount conf
  } 

getSiteAndAccount :: Text -> Text -> IO (Site, Account)
getSiteAndAccount siteNick loginName = do
  homeDir <- getHomeDirectory
  confStr <- readFile $ homeDir ++ "/.config/amkhlv/websoup.json"
  let conf = fromJust (( decode $ C8.pack confStr ) :: Maybe ConfSecrets)
  sites <- getSecrets (secretFile conf) ""
  let s = Prelude.head $ Prelude.filter (\site -> nick site == siteNick) sites
  let a = Prelude.head $ Prelude.filter (\account -> login account == loginName) $ accounts s
  return (s,a)

getPassword :: Text -> Text -> IO String
getPassword siteNick loginName = do 
  (_,a) <- getSiteAndAccount siteNick loginName
  return . unpack $ password a

type Things = [(String, String, WD ())]

printThings :: Things -> IO ()
printThings things = sequence_ [ do
  setSGR [SetColor Foreground Vivid Green, SetConsoleIntensity BoldIntensity]
  putStr c 
  setSGR [Reset] 
  putStrLn $ " → " ++ s 
  | (c,s,_) <- things
  ]
askWhatNext :: Things -> WD ()
askWhatNext things = do
  c <- IOC.liftIO $ printThings things >> getLine
  IOC.liftIO $ print c
  sequence_ [ 
    IOC.liftIO (putStrLn y) >> a 
    | (x,y,a) <- things, x == c 
    ]