{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}


module Weather where


import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import qualified Network.HTTP.Simple as S
import           GHC.Generics
import qualified Data.Text as T
import           Data.Aeson
import           Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as C8
import           System.Directory

data ConfWeather = ConfWeather {
    url :: String
  , dumpTo :: String
} deriving (Generic, Show)

instance ToJSON ConfWeather where toEncoding = genericToEncoding defaultOptions
instance FromJSON ConfWeather

main :: IO ()
main = do
  homeDir <- getHomeDirectory
  confStr <- readFile $ homeDir ++ "/.config/amkhlv/websoup/weather.json"
  let conf = fromJust (( decode $ C8.pack confStr ) :: Maybe ConfWeather)

  man <- newManager tlsManagerSettings
  initReq <- parseRequest $ url conf
  let req = initReq {method="GET"}
  res <- httpLbs req man
  writeFile (dumpTo conf) (C8.unpack $ S.getResponseBody res)


