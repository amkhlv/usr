{-# LANGUAGE Arrows #-}
module Main where

import           System.IO
import qualified System.Process as SP
import           Data.List
import           Data.List.Split
import           Text.Regex.PCRE
import qualified Data.ByteString.Char8 as C
import qualified Data.Array as DA
import           Text.XML.HXT.Core
import           Text.XML.HXT.RelaxNG
import           Text.XML.HXT.Arrow.Pickle
import           Data.Map
import           Data.Maybe

data Window = Window {
  nick :: String,
  geometry :: String
  }
instance XmlPickler Window where
  xpickle = xpWindow
xpWindow :: PU Window
xpWindow =
  xpElem "window" $
  xpWrap ( \ ((nic, geo)) -> Window nic geo
         , \t -> (nick t, geometry t)
         ) $
  xpPair (xpAttr "nick" xpText) (xpAttr "geometry" xpText)

getWindowList :: String -> IO [Maybe Window]
getWindowList r  = runX $ readDocument [withRemoveWS yes] "/home/andrei/.config/amkhlv/wint.xml"  >>>
  getChildren >>>
  getChildren >>>
  hasAttrValue "resolution" (\x -> x == r) >>>
  getChildren >>^
  unpickleDoc xpWindow

winsToMap :: [Maybe Window] -> Map String String
winsToMap ws = fromList [ (nick (fromJust w), geometry (fromJust w)) | w <- ws , isJust w ]

prepRegex :: String -> Regex
prepRegex x = makeRegex (C.pack x)

getScreenRes :: IO (String, Handle, Handle)
getScreenRes = do
  (Just hin, Just hout, Just herr, procHandle) <- SP.createProcess (
    SP.proc
      "/bin/bash"
      ["-c", "xrandr"]){SP.std_in = SP.CreatePipe, SP.std_out = SP.CreatePipe, SP.std_err = SP.CreatePipe}
  hClose hin
  o <- hGetContents hout
  let r = C.pack "current\\s+(\\d+)\\s+x\\s+(\\d+)"
  let ms :: DA.Array Int (MatchText String)
      ms = getAllTextMatches $ o =~ r
  return $ ((fst ((ms DA.! 0) DA.! 1)) ++ "x" ++ (fst ((ms DA.! 0) DA.! 2)), hout, herr)
  
strToInt :: String -> Int
strToInt x = read x

execStr :: String -> IO ()
execStr x = do
  (Just hin, Just hout, Just herr, procHandle) <- SP.createProcess  (
    SP.proc
      "/bin/bash"
      ["-c", x]){SP.std_in = SP.CreatePipe, SP.std_out = SP.CreatePipe, SP.std_err = SP.CreatePipe}
  o <- hGetContents hout
  putStrLn o
  e <- hGetContents herr
  putStrLn e
  hClose hin >> hClose hout >> hClose herr

main :: IO ()
main = do
  (r, hout1, herr1) <- getScreenRes
  ws <- getWindowList r
  let wmap = winsToMap ws
  (Just hin, Just hout, Just herr, procHandle) <- SP.createProcess  (
    SP.proc
      "/bin/bash"
      ["-c", "wmctrl -l"]){SP.std_in = SP.CreatePipe, SP.std_out = SP.CreatePipe, SP.std_err = SP.CreatePipe}
  hClose hin
  o <- hGetContents hout
  let lns  = Data.List.map (\y -> (Data.List.Split.split (condense . dropDelims $ oneOf " ") y)) $ Data.List.filter (\x -> (length x) > 0) (splitOn "\n" o)
  let rs   = [ head x | x <- lns ]
  let dhts = [ tail x | x <- lns ] -- [[desktop, host, title]]
  y <- hGetContents herr
  sequence_ [ putStr (show (fst z)) >>
              putStr " : " >>
              putStrLn (intercalate " " $ tail $ tail (snd z)) | z <- zip [0..] dhts ]
  putStrLn "" >> putStr "Available Keys: " >> sequence_ [ putStr (case w of
                                                                    Just x -> (nick x) ++ " "
                                                                    Nothing -> "--") | w <- ws ]
  putStrLn y
  s <- getLine
  let usels = [(Data.List.Split.split (condense . dropDelims $ oneOf ":,.") x) |
               x <- (Data.List.Split.split (condense . dropDelims $ oneOf " ") s)]
  putStrLn $ show usels
  sequence_ [execStr ("wmctrl -i -r " ++ (rs !! (strToInt (iname !! 0))) ++ " -e 0," ++ (wmap ! (iname !! 1))) | iname <- usels]
  hClose hout >> hClose herr >> hClose hout1 >> hClose herr1
