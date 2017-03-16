
module Main where
import           Options.Applicative
import           Data.Monoid
import           Control.Monad
import           Graphics.UI.Gtk 
import           Data.Word 
import           Data.Maybe
import           Control.Concurrent
import           Network.HTTP.Simple
import           Control.Exception
import qualified Data.ByteString.Lazy as DBSL
import           Text.XML.HXT.Core
import           Text.XML.HXT.RelaxNG
import           Text.XML.HXT.Arrow.Pickle
import           System.Directory

indicatorRNG :: String
indicatorRNG = ".config/amkhlv/indicator.rng"

-- command line parsing --

data Clops = Clops { configXML :: String }

cloparser :: Parser Clops
cloparser = Clops <$> ( argument str (metavar "XML-config-file"))

-- define XML schema  --

data Conf = Conf {
    size :: Int
  , even :: (Int, Int, Int)
  , odd :: (Int, Int, Int)
  , toCheck :: [CheckURL]
  }
instance XmlPickler Conf where
  xpickle = xpConf
xpConf :: PU Conf
xpConf =
  xpElem "config" $
  xpWrap (uncurry4 Conf, \z -> (Main.size z, Main.even z, Main.odd z, toCheck z)) $
  xp4Tuple
    (xpElem "size" xpInt)
    (xpElem "even" $ xpTriple (xpAttr "r" xpInt) (xpAttr "g" xpInt) (xpAttr "b" xpInt))
    (xpElem "odd"  $ xpTriple (xpAttr "r" xpInt) (xpAttr "g" xpInt) (xpAttr "b" xpInt))
    (xpElem "servers" $ xpList xpCheckURL)

data LED = LED {
    x :: Int
  , y :: Int
  , r :: Int
  , g :: Int
  , b :: Int
  }
instance XmlPickler LED where
  xpickle = xpLED
xpLED :: PU LED
xpLED =
  xpElem "LED" $
  xpWrap (\(x0,y0,r0,g0,b0) -> LED x0 y0 r0 g0 b0, \u -> (x u, y u, r u, g u, b u)) $
  xp5Tuple
    (xpAttr "x" $ xpInt)
    (xpAttr "y" $ xpInt)
    (xpAttr "r" $ xpInt)
    (xpAttr "g" $ xpInt)
    (xpAttr "b" $ xpInt)

data CheckURL = CheckURL {
    url :: String
  , evenLEDs :: [LED]
  , oddLEDs  :: [LED]
  , showIfDown :: Maybe String
  }
instance XmlPickler CheckURL where
  xpickle = xpCheckURL
xpCheckURL :: PU CheckURL
xpCheckURL =
  xpElem "server" $
  xpWrap (uncurry4 CheckURL, \x0 -> (url x0, evenLEDs x0, oddLEDs x0, showIfDown x0)) $
  xp4Tuple
    (xpAttr "URL" xpText)
    (xpElem "even" $ xpList xpLED)
    (xpElem "odd"  $ xpList xpLED)
    (xpOption (xpElem "showIfDown" xpText0))


-- graphics --

smallPB :: Conf -> Word8 -> Word8 -> Word8 -> IO Pixbuf
smallPB conf rw gw bw = do
  pb <- pixbufNew ColorspaceRgb False 8 (size conf) (size conf)
  pixbufFill pb rw gw bw 0
  return pb

lookupRGB :: Int -> Int -> [LED] -> Maybe (Word8,Word8,Word8)
lookupRGB x0 y0 leds = listToMaybe [ (fromIntegral (r l), fromIntegral (g l), fromIntegral (b l))
                                   | l <- leds , x l == x0, y l == y0 ]

defaultColor :: Conf -> Bool -> (Word8, Word8, Word8)
defaultColor conf isEven = let (r0,g0,b0) = if isEven then (Main.even conf) else (Main.odd conf) in
  (fromIntegral r0, fromIntegral g0, fromIntegral b0)

indicator :: Conf -> [LED] -> Bool -> IO Pixbuf
indicator conf leds isEven = do
  pb <- pixbufNew ColorspaceRgb False 8 (2 * (size conf)) (2 * (size conf))
  pixbufFill pb 0 0 0 0
  sequence_  [ do
                 spb <- uncurry3 (smallPB conf) $ fromMaybe (defaultColor conf isEven) (lookupRGB x0 y0 leds) 
                 pixbufScale spb pb
                   (x0 * (size conf))  (y0 * (size conf))  (size conf)  (size conf) 
                   (fromIntegral (x0 * (size conf))) (fromIntegral (y0 * (size conf)))  1   1
                   InterpNearest
             | x0 <- [0,1] , y0 <- [0,1] ] 
  evaluate pb

updatePB :: Bool -> StatusIcon -> MVar (IO Pixbuf) -> MVar (IO Pixbuf) -> IO ()
updatePB n icn mvEven mvOdd = do
  threadDelay 500000
  if n
    then do
    pb <- readMVar mvEven
    postGUIAsync $ pb   >>=   statusIconSetFromPixbuf icn
    else do
    pb <- readMVar mvOdd
    postGUIAsync $ pb   >>=   statusIconSetFromPixbuf icn
  updatePB (not n) icn mvEven mvOdd

mkmenu :: MVar () -> IO Menu
mkmenu mv = do
  m <- menuNew
  mapM_ (mkitem m) [("Quit"::String, enough mv)]
  return m
    where
        mkitem menu (label,act) =
            do i <- menuItemNewWithLabel label
               menuShellAppend menu i
               on i menuItemActivated act        

-- check URL --

checkURL :: Request -> IO Bool
checkURL u = do
  eresp <- try $ httpLBS $ u :: IO (Either SomeException (Response DBSL.ByteString))
  case eresp of
    Left _ -> evaluate False
    Right resp ->
      case getResponseStatusCode resp of
        200 -> evaluate True
        _   -> evaluate False

xor :: Bool -> Bool -> Bool
xor a0 b0 = (a0 || b0) && not (a0 && b0)

mainLoop :: Conf -> MVar (IO Pixbuf) -> MVar (IO Pixbuf) -> IO ()
mainLoop conf mvEven mvOdd = do
  ledsEvenOdd <- sequence [
    let doInvert =
          case (showIfDown srv) of
            Just _  -> True
            Nothing -> False
    in
      do
        initReq <- parseRequest (url srv)
        serverIsUp <- checkURL initReq
        if (doInvert `xor` serverIsUp)
          then return ([ LED (x l) (y l) (r l) (g l) (b l) | l <- evenLEDs srv],
                       [ LED (x l) (y l) (r l) (g l) (b l) | l <- oddLEDs srv ])
          else return ([],[])
    | srv <- toCheck conf ]
  modifyMVar mvEven $ const $ (,) <$> evaluate (indicator conf (join $ Prelude.map fst ledsEvenOdd) True) <*> return ()
  modifyMVar mvOdd  $ const $ (,) <$> evaluate (indicator conf (join $ Prelude.map snd ledsEvenOdd) False) <*> return ()
  threadDelay 60000000
  mainLoop conf mvEven mvOdd


enough :: MVar () -> IO ()
enough mv = do
  postGUIAsync mainQuit
  putMVar mv ()

-- read XML configuration file --

getConf :: Clops -> IO Conf
getConf clops = do
  home <- getHomeDirectory
  xs <- runX $
    readDocument [withRelaxNG (home ++ "/" ++ indicatorRNG), withRemoveWS yes] (configXML clops) >>^ unpickleDoc xpConf
  case Prelude.head xs of
    Just c -> return c
    Nothing -> error "unable to read configuration"

-- main --

main :: IO ()
main = do
  let opts = info (helper <*> cloparser)
             ( fullDesc
               <> progDesc "server status indicaror"
               <> header "indicate if servers are up or down" )
  clops <- execParser opts
  conf <- getConf clops
  mylock <- newEmptyMVar
  _ <- initGUI
  pic0  <- indicator conf [] True
  pic1  <- indicator conf [] False
  statusPB0 <- newMVar $ evaluate pic0
  statusPB1 <- newMVar $ evaluate pic1
  icon <- statusIconNewFromPixbuf  pic0
  menu <- mkmenu mylock
  _ <- on icon statusIconPopupMenu $ \b0 a0 -> do
    widgetShowAll menu
    print (b0,a0)
    menuPopup menu $ maybe Nothing (\b1 -> Just (b1,a0)) b0
  statusIconSetVisible icon True
  _ <- forkIO $ mainLoop conf statusPB0 statusPB1 
  _ <- forkIO $ updatePB True icon statusPB0 statusPB1
  _ <- forkOS mainGUI
  _ <- takeMVar mylock
  return ()
