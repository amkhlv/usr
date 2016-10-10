{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import GHC.Generics
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Network.Http.Client
import Data.Text
import Data.List
import Data.Maybe
import Data.Aeson
import Data.Array
import Data.Time
import Options.Applicative
import qualified Blaze.ByteString.Builder.ByteString as BBBB
import Text.Regex.PCRE
import qualified Data.ByteString.Char8 as C
import System.Directory
import Control.Monad
import System.Console.ANSI


data Clops = Clops
             { start :: Maybe String
             , end :: Maybe String
             , forward :: Maybe Int
             }

cloparser :: Parser Clops
cloparser = Clops
  <$> optional (option str ( short 'a' <> metavar "A" <> help "after YYYY-MM-DD"))
  <*> optional (option str ( short 'b' <> metavar "B" <> help "before YYYY-MM-DD"))
  <*> optional (option auto ( short 'n' <> metavar "N" <> help "number of day to look forward"))

data ANodeConf = ANodeConf {
  socket :: String
  , calendar :: String
  } deriving (Generic, Show)
instance ToJSON ANodeConf
instance FromJSON ANodeConf

data IntervalRequest = IntervalRequest {
  isRequestInterval :: Bool,
  dateFrom :: ICALDate,
  dateUntil  :: ICALDate
  } deriving (Generic, Show)
instance ToJSON IntervalRequest
instance FromJSON IntervalRequest

data VEvent = VEvent {
  summary :: String
  , location :: Maybe String
  , description :: Maybe String
  , duration :: Maybe Duration
  , organizer :: Maybe String
  , endDate :: Maybe ICALDate
  , startDate :: Maybe ICALDate
  , isRecurring :: Bool
  , isDerived :: Bool
  } deriving (Generic, Show)
instance ToJSON VEvent
instance FromJSON VEvent

data Duration = Duration {
  weeks :: Int,
  days :: Int,
  hours :: Int,
  minutes :: Int,
  seconds :: Int,
  isNegative :: Bool
  } deriving (Generic, Show)
instance ToJSON Duration
instance FromJSON Duration

data ICALDate = ICALDate {
  year :: Int,
  month :: Int,
  day :: Int,
  hour :: Int,
  minute :: Int,
  second :: Int,
  isDate :: Bool
  } deriving (Generic, Show)
instance ToJSON ICALDate
instance FromJSON ICALDate

showYYYYMMDD :: Int -> Int -> Int -> String
showYYYYMMDD y m d =
  (show y) ++ "-" ++ (if (m<10) then "0" ++ (show m) else (show m)) ++ "-" ++ (if (d<10) then "0" ++ (show d) else (show d))

showHHMM :: Int -> Int -> String
showHHMM h m =
  (show h) ++ ":" ++ (if (m < 10) then "0" ++ (show m) else (show m))

showICALDate :: ICALDate -> String
showICALDate icd = let symd = (showYYYYMMDD (year icd)  (month icd)  (day icd))
                       shm  = (showHHMM (hour icd) (minute icd)) in
  case (isDate icd) of
    True  -> symd
    False -> symd ++ (if ((hour icd) < 10) then "  " else " ") ++ shm

icaldate2int :: ICALDate -> Integer
icaldate2int icd = toInteger(64*32*32*16)*(toInteger $ year icd)
  + toInteger(64*32*32*(month icd) + 64*32*(day icd) + 64*(hour icd) + (minute icd))

s2i :: String -> Int
s2i x = read x

parseYYYYDDMM :: String -> (Int, Int, Int)
parseYYYYDDMM x =
  let r = C.pack "(\\d\\d\\d\\d)-(\\d\\d)-(\\d\\d)" in
  let ms :: Data.Array.Array Int (MatchText String)
      ms = getAllTextMatches $ x =~ r in
    (s2i $ fst ((ms ! 0) ! 1)  , s2i $ fst ((ms ! 0) ! 2) , s2i $ fst ((ms ! 0 ) ! 3))
  -- http://stackoverflow.com/questions/5591192/grouping-in-haskell-regular-expressions

showVEvent :: VEvent -> IO ()
showVEvent x = do
  case (startDate x) of
    Just sd -> (setSGR [SetColor Foreground Dull Cyan] >> (putStr $ (showICALDate sd) ++ "   ") >> setSGR [Reset]) 
    Nothing -> return ()
  putStr $ summary x
  case (location x) of
    Just l -> putStr $ " ( " ++ l ++ " ) "
    Nothing -> return ()
  if (isRecurring x) then putStr "  RCR " else return ()
  if (isDerived x) then putStr " DRV " else return ()
  putStrLn "\n"

listInputStream :: InputStream BS.ByteString -> [VEvent] -> IO [VEvent]
listInputStream i xs = do
  xm <- Streams.read i
  case xm of
    Just x -> case (decode $ BSL.fromStrict x) of
      Just vev -> listInputStream i (vev:xs)
      Nothing -> (putStrLn "ERROR" >> listInputStream i xs)
    Nothing -> return $ Prelude.reverse xs

showEvents :: [VEvent] -> IO ()
showEvents evs = sequence_ [showVEvent ev | ev <- evs]

readANodeConf :: IO ANodeConf
readANodeConf = do
  home <- getHomeDirectory
  let confFile = home ++ "/.config/amkhlv/anode.json"
  jsn <- readFile confFile
  case (decode $ BSL.fromStrict $ C.pack jsn) of
    Just x -> return x
    Nothing -> error $ "Unable to decode " ++ confFile

processEvents :: ANodeConf -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
processEvents anconf ya ma da yb mb db = do
  c <- openConnectionUnix (socket anconf)
  let q = buildRequest1 $ do
        http POST "/"
        setAccept "text/html"
  sendRequest c q (\o ->
                     Streams.write
                    (Just (BBBB.fromByteString $ BSL.toStrict $ encode (IntervalRequest
                                                                        True
                                                                        (ICALDate ya ma da 0 0 0 False)
                                                                        (ICALDate yb mb db 0 0 0 False))))
                    o)
  receiveResponse c (\p i -> do
                        putStrLn ""
                        -- putStrLn $ show p
                        evs <- listInputStream i [] 
                        showEvents $ sortOn
                          (\ev -> case (startDate ev) of
                                    Nothing -> toInteger(64*32*32*16) * toInteger(4096)
                                    Just x -> icaldate2int x)
                          evs
                    )
  closeConnection c

dispatch :: Clops -> ANodeConf -> IO ()
dispatch (Clops  (Just dateAfter)  (Just dateBefore) Nothing) anconf  =
  let (ya, ma, da) = parseYYYYDDMM dateAfter 
      (yb, mb, db) = parseYYYYDDMM dateBefore in
    processEvents anconf ya ma da yb mb db
dispatch (Clops Nothing Nothing (Just n)) anconf = do
  (t,tz) <- (,) <$> getCurrentTime <*> getCurrentTimeZone
  let lDay = localDay (utcToLocalTime tz t)
  let (y, m, d) = toGregorian lDay
  let (y1, m1, d1) = toGregorian (addDays (toInteger n) lDay)
  processEvents anconf (fromIntegral y) m d (fromIntegral y1) m1 d1
dispatch (Clops (Just dateAfter) Nothing (Just n)) anconf = do
  let (y, m, d) = parseYYYYDDMM dateAfter
  let lDay = fromGregorian (toInteger y) m d
  let (y1, m1, d1) = toGregorian (addDays (toInteger n) lDay)
  processEvents anconf (fromIntegral y) m d (fromIntegral y1) m1 d1
dispatch _ _ = putStrLn "Incompatible options"

main :: IO ()
main = join $ dispatch <$> (execParser opts) <*> readANodeConf
  where
    opts = info (helper <*> cloparser)
           ( fullDesc
             <> progDesc "shows events"
             <> header "calendar monitor" )
