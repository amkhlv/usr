{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import qualified Data.Yaml as Y
import qualified System.Directory as SD
import qualified System.FilePath as FP
import qualified Data.Time.Clock as CLK
import qualified Data.Text as T
import GHC.Generics
import Graphics.Svg
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Format
import qualified System.IO as SIO

data YamlConf = YamlConf {
  weeks :: Int,
  dayW :: Int,
  dayH :: Int,
  daySize :: Int,
  output :: String
} deriving (Show, Generic)
instance Y.FromJSON YamlConf

data Config = Config {
  nWeeks :: Int,
  dayWidth :: Int,
  dayHeight :: Int,
  dayFontSize :: Int,
  startDay :: Day
}
tshow = T.pack . show
getYamlConf :: FilePath -> IO (Either Y.ParseException YamlConf)
getYamlConf h = Y.decodeFileEither $ FP.combine h ".config/amkhlv/SVGCalendar.yaml"
monthNames :: [String]
monthNames = [ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
svg :: (Config -> Element) -> Config -> Element
svg content cnf =
     doctype
  <> with (svg11_ $ content cnf) [
    Version_ <<- "1.1"
    , Width_ <<- tshow (dayWidth cnf * 10)
    , Height_ <<- tshow (dayHeight cnf * (nWeeks cnf + 2))
    ]
day2Elem :: Day -> Element
day2Elem d = let (_, _, x) = toGregorian d in toElement (show x)
day2dm :: Day -> Int
day2dm d = let (_, _, x) = toGregorian d in x
day2m :: Day -> Int
day2m d = let (_, m, _) = toGregorian d in m
day :: Config -> Int -> Int -> Day -> Element
day cnf x y d = rect_ [
  X_ <<- tshow x
  , Y_ <<- tshow y
  , Width_ <<-  tshow (dayWidth cnf)
  , Height_ <<- tshow (dayHeight cnf)
  , Fill_ <<- "white"
  , Stroke_ <<- "black"
  ] <> text_ [
  X_ <<- tshow (quot (dayHeight cnf) 10 + x)
  , Y_ <<- tshow (quot (dayHeight cnf) 3 + y)
  , Font_size_ <<- tshow (dayFontSize cnf)
  ] (day2Elem d)
week :: Config -> Int -> Int -> Day -> Element
week cnf _ y sd =
  mconcat $ [day cnf (dayWidth cnf * (x + 1)) (dayHeight cnf * y) (addDays (toInteger x) sd) | x <- [0 .. 6]] ++
  [ text_
    [ X_ <<- tshow (quot (dayWidth cnf) 10)
    , Y_ <<- tshow (quot (dayHeight cnf) 3 + y * dayHeight cnf)
    , Font_size_ <<- tshow (dayFontSize cnf)
    ]
    (toElement (monthNames !! (day2m sd - 1)))
  | day2dm sd < 8
  ] ++ 
  [ text_
    [ X_ <<- tshow (quot (dayWidth cnf) 10)
    , Y_ <<- tshow (quot (dayHeight cnf) 3 + 2 * dayFontSize cnf + y * dayHeight cnf)
    , Font_size_ <<- tshow (dayFontSize cnf)
    ]
    (toElement (tshow $ let (yyyy, _, _) = toGregorian sd in fromInteger yyyy))
  | day2dm sd < 8
  ]
contents :: Config -> Element
contents cnf = 
  text_ [
        X_ <<- tshow (dayWidth cnf)
        , Y_ <<- tshow (quot (dayHeight cnf) 2)
        , Font_size_ <<- "16"
    ] "Events are on top layer. In Inkscape, go to top layer then Ctrl-A to select all objects." 
  <>
  text_ [
        X_ <<- tshow (dayWidth cnf)
        , Y_ <<- tshow ( dayHeight cnf - 4 )
        , Font_size_ <<- "16"
    ] "SUNDAY"
  <> 
  text_ [
        X_ <<- tshow (7 * dayWidth cnf)
        , Y_ <<- tshow ( dayHeight cnf - 4 )
        , Font_size_ <<- "16"
    ] "SATURDAY"
  <> 
  mconcat [
        week cnf 1 (w + 1) (addDays (toInteger $ 7 * w) (sunday $ startDay cnf))| w <- [0 .. (nWeeks cnf - 1)]
    ]
sunday :: Day -> Day
sunday d = let (_, _, dow) = toWeekDate d in if dow == 7 then d else addDays (toInteger $ - dow) d

main :: IO ()
main = do 
    home <- SD.getHomeDirectory
    eeConf <- getYamlConf home
    timeNow <- CLK.getCurrentTime
    case eeConf of
        Left _ -> putStrLn "ERROR: cannot read configuration file"
        Right (YamlConf nw dw dh ds p) -> let conf = Config nw dw dh ds (CLK.utctDay timeNow) in 
            do 
                fh <- SIO.openFile p SIO.WriteMode 
                TLIO.hPutStr fh (prettyText $ svg contents conf)
                SIO.hClose fh