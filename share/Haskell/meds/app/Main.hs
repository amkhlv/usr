{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.Pickle
import qualified Data.Text as T
import Data.List
import Data.Maybe (fromJust)
import qualified Control.Monad as C
import Control.Monad.IO.Class
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Lib


data Clops = Clops
  { xmlFile :: String
  , currentOnly :: Bool
  }

cloparser :: Parser Clops
cloparser = Clops
  <$> argument str (metavar "XML")
  <*> switch ( long "cur" <> help "Current only" )

data Med =  Med
  { med :: T.Text
  , note :: Maybe T.Text
  , brands :: [Brand]
  } deriving (Show)
instance XmlPickler Med where
  xpickle = xpMed
xpMed :: PU Med
xpMed = xpElem "medicine" $
  xpWrap ( \((nm, nt, br)) -> Med (T.pack nm) (T.pack <$> nt) br 
         , \m -> (T.unpack $ med m, T.unpack <$> note m, brands m)
         ) $
  xpTriple
  (xpAttr "name" xpText)
  (xpOption $ xpElem "note" xpXmlText)
  (xpElem "brands" $ xpList xpBrand)

data Tag = Current | Old | Dubious deriving (Show, Eq)
getTags :: Maybe T.Text -> [Tag]
getTags Nothing = []
getTags (Just t) = concat
  [ if "<cur/>" `T.isInfixOf` t then [Current] else []
  , if "<q/>"  `T.isInfixOf` t then [Dubious] else []
  , if "<old/>" `T.isInfixOf` t then [Old] else [] 
  ]

data Brand = Brand
  { brandName :: T.Text
  , started :: Maybe T.Text
  , ended :: Maybe T.Text
  , brandNote :: Maybe T.Text
  , brandTags :: [Tag]
  } deriving (Show)
xpBrand :: PU Brand
xpBrand = xpElem "brand" $
  xpWrap ( \((nm,fr,ds,nt,ts)) ->
             Brand (T.pack nm) (T.pack <$> fr) (T.pack <$> ds) (T.pack <$> nt) (getTags $ T.pack <$> ts)
         , \b ->
             (T.unpack $ brandName b, T.unpack <$> started b, T.unpack <$> ended b, T.unpack <$> brandNote b, Nothing)
         ) $
  xp5Tuple
  (xpAttr "name" xpText)
  (xpOption $ xpAttr "started" xpText)
  (xpOption $ xpAttr "ended" xpText)
  (xpOption $ xpElem "note" xpXmlText)
  (xpOption $ xpElem "tags" $ xpXmlText)

atTag tag = deep (isElem >>> hasName tag)

brand2Html :: Brand -> H.Html
brand2Html b = do
  (H.toHtml $ brandName b)
  case brandNote b of
    Just n -> H.preEscapedText n
    Nothing -> return ()
  H.toHtml $ show $ brandTags b


med2Html :: Med -> H.Html
med2Html m = do
  H.h4 $ H.toHtml (med m)
  case note m of
    Just nt -> H.preEscapedText nt
    Nothing -> return ()
  H.ul $ C.forM_ (brands m) (H.li . brand2Html)

meds2Html :: [Med] -> H.Html
meds2Html meds = H.docTypeHtml $  do
  H.head $ do
    (H.meta H.! A.charset  "UTF-8")
    (H.title "Лекарства")
  H.body $ do
    H.ul $ C.forM_ meds (H.li . med2Html)

filterMeds :: Clops -> [Med] -> [Med]
filterMeds clops meds = do
  Med nm nt bs <- meds
  let curbrands =
        if (currentOnly clops)
        then [b | b <- bs , Current `elem` (brandTags b)]
        else bs
  case curbrands of
    [] -> []
    _  -> return $ Med nm nt curbrands
        

main :: IO ()
main = do
  let  opts = info (helper <*> cloparser)
              ( fullDesc
                <> progDesc "medications organizer"
                <> header "query list of medications" )
  clops <- execParser opts

  ms <- runX $ readDocument [ withRemoveWS yes ] (xmlFile clops)
        >>>
        deep (isElem >>> hasName "medicine")
        >>^ (fromJust  . unpickleDoc xpMed)
  let meds = filterMeds clops ms
  putStrLn $ renderHtml $ meds2Html meds
