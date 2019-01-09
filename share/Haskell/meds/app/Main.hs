{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.Pickle
import qualified Data.Text as T
import Data.List
import Data.List.Split (splitWhen)
import Data.Maybe (fromJust)
import qualified Data.Monoid as MI
import qualified Control.Monad as C
import Control.Monad.IO.Class
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Lib

data Clops = Clops
  { xmlFile :: String
  , day :: Maybe String
  , clopName :: Maybe String
  , currentOnly :: Bool
  }

cloparser :: Parser Clops
cloparser = Clops
  <$> argument str (metavar "XML")
  <*> optional ( option str ( short 'd' <> metavar "Day" <> help "taken on this day" ) )
  <*> optional ( option str ( short 'n' <> metavar "Name" <> help "either generic or brand"))
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
  xpWrap ( \((nm, nt, br)) -> Med { med = T.pack nm
                                  , note = T.pack <$> nt
                                  , brands = br
                                  }
         , \m -> (T.unpack $ med m, T.unpack <$> note m, brands m)
         )
  $
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

data Period = Period
  { start :: Maybe T.Text
  , end :: Maybe T.Text
  , periodNote :: T.Text
  } deriving Show
instance XmlPickler Period where
  xpickle = xpPeriod
xpPeriod  :: PU Period
xpPeriod = xpElem "period" $
  xpWrap ( \((s,e,n)) -> Period { start = T.pack <$> s
                                , end = T.pack <$> e
                                , periodNote = T.pack n
                                }
         , \p -> (T.unpack <$> start p, T.unpack <$> end p, T.unpack $ periodNote p)
         ) $
  xpTriple
  (xpOption $ xpAttr "start" xpText)
  (xpOption $ xpAttr "end" xpText)
  xpXmlText

data Brand = Brand
  { brandName :: T.Text
  , brandNote :: Maybe T.Text
  , periods :: [Period]
  , brandTags :: [Tag]
  } deriving (Show)
xpBrand :: PU Brand
xpBrand = xpElem "brand" $
  xpWrap ( \((nm,nt,mps,ts)) ->
             Brand { brandName = T.pack nm
                   , brandNote = T.pack <$> nt
                   , periods = case mps of { Just ps -> ps ; Nothing -> [] }
                   , brandTags = getTags $ T.pack <$> ts
                   }
         , \b ->
             (T.unpack $ brandName b
             , T.unpack <$> brandNote b
             , Nothing -- TODO
             , Nothing -- TODO
             )
         ) $
  xp4Tuple
  (xpAttr "name" xpText)
  (xpOption $ xpElem "note" xpXmlText)
  (xpOption $ xpElem "periods" $ xpList xpickle)
  (xpOption $ xpElem "tags" $ xpXmlText)

atTag tag = deep (isElem >>> hasName tag)

period2Html :: Period -> H.Html
period2Html p = do
  case (start p, end p) of
    (Just s, Just e) -> H.toHtml $ T.concat [ s, " → ", e ]
    (Just s, Nothing) -> H.toHtml $ T.concat [ s, " → ..."]
    (Nothing, Just e) -> H.toHtml $ T.concat [ "... → ", e]
    (Nothing, Nothing) -> return ()
  case periodNote p of
    "" -> return ()
    t  -> H.br >> H.preEscapedText t

brand2Html :: Brand -> H.Html
brand2Html b = do
  (H.toHtml $ brandName b)
  case brandNote b of
    Just n -> H.preEscapedText n
    Nothing -> return ()
  H.toHtml $ show $ brandTags b
  H.ul $ C.forM_ (periods b) period2Html

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

filterCur :: Clops -> [Med] -> [Med]
filterCur clops meds = if currentOnly clops
  then meds >>= \(Med nm nt bs) ->
                  case [b | b <- bs , Current `elem` (brandTags b)] of
                    [] -> []
                    x  -> [Med nm nt x]
  else meds

filterDay :: Clops -> [Med] -> [Med]
filterDay clops meds = case day clops of
  Nothing -> meds
  Just d  -> meds >>= \(Med nm nt bs) ->
    let tst b = case periods b of
                  [] -> True
                  ps -> MI.getAny $ MI.mconcat [MI.Any $ d `isWithin` p | p <- ps]
    in
    case filter tst bs of { [] -> [] ; x -> [Med nm nt x] }

filterName :: Clops -> [Med] -> [Med]
filterName clops meds = case clopName clops of
  Nothing -> meds
  Just nm -> meds >>= \(Med n nt bs) ->
    if T.pack nm `T.isInfixOf` n
    then [Med n nt bs]
    else case filter (\b -> T.pack nm `T.isInfixOf` brandName b) bs of
      [] -> []
      matchingBrands -> [Med n nt matchingBrands]

filterMeds :: Clops -> [Med] -> [Med]
filterMeds clops meds =
  filterName clops $ filterDay clops $ filterCur clops $ meds
        
dateStrToInt :: String -> Int
dateStrToInt d = read $ concat $ splitWhen (== '-') d

isWithin :: String -> Period -> Bool
isWithin d p = let i = dateStrToInt d in
  case (dateStrToInt . T.unpack <$> start p, dateStrToInt . T.unpack <$> end p) of
    (Nothing, Nothing) -> True
    (Nothing, Just x)  -> i <= x
    (Just x, Nothing)  -> i >= x
    (Just x, Just y)   -> (i >= x) && (i <= y)

main :: IO ()
main = do
  let  opts = info (helper <*> cloparser)
              ( fullDesc
                <> progDesc "medications organizer"
                <> header "query list of medications" )
  clops <- execParser opts

  ms <- runX ( readDocument [ withRemoveWS yes ] (xmlFile clops)
               >>>
               deep (isElem >>> hasName "medicine")
               >>^
               (fromJust . unpickleDoc xpMed)
             )
  let meds = filterMeds clops ms
  putStrLn $ renderHtml $ meds2Html meds
