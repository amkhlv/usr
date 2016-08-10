{-# LANGUAGE DeriveGeneric #-}

module Handler.Home where

import Import
import           Data.Maybe
import qualified Data.Conduit.List as DCL
import Database.Persist.Sql (fromSqlKey)
import           Database.Persist.Sql (toSqlKey)
import qualified Data.ByteString.Lazy as DBSL
import qualified Data.Aeson as A
import           System.Process
import Handler.Utils


conduit :: Text -> Maybe Text -> ConduitM (Entity Address) (Entity Address) (ReaderT SqlBackend (HandlerT App IO)) ()
conduit lastname firstname =
  DCL.filter
  (\a ->
    let lastNameMatches = lastname `isInfixOf` (addressLast (entityVal a))
        firstNameMatches = (fromMaybe "" firstname) `isInfixOf` (fromMaybe "" (addressFirst (entityVal a)))         
    in lastNameMatches && firstNameMatches
  )

getHomeR :: Handler Html
getHomeR = do
    (searchWidget, searchEnctype) <- generateFormPost searchForm
    defaultLayout $ do
        setTitle "Abk"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((searchResult, searchWidget), searchEnctype) <- runFormPost searchForm
    let a = case searchResult of
            FormSuccess res -> Just res
            _ -> Nothing
    s <- runDB $
      sourceToList $ (selectSource [] []) =$= (conduit (searchPersonLast (fromJust a)) (searchPersonFirst (fromJust a)))
    defaultLayout $ do
        setTitle "Abk"
        $(widgetFile "homepage")
        sequence_ [ $(widgetFile "address") | m <- s ]

searchForm :: Html -> MForm Handler (FormResult SearchPerson, Widget)
searchForm = renderDivs $ SearchPerson
  <$> areq textField "Last Name" Nothing
  <*> aopt textField "First Name" Nothing

convertJSONtoVCF :: ByteString -> IO (Handle, Handle, ProcessHandle, ByteString)
convertJSONtoVCF jsn = do
  (Just hin, Just hout, Just herr, procHandle) <- createProcess  (
    proc
      "/bin/bash"
      ["-c", "/usr/local/lib/amkhlv/json2vcard.py"]){
    std_in = CreatePipe,
    std_out = CreatePipe,
    std_err = CreatePipe}
  hPutStrLn hin jsn
  hClose hin
  x <- hGetContents hout
  return (hout, herr, procHandle, x)

vCardContentType :: ByteString
vCardContentType = "text/x-vcard"

getVCardR :: Int64 -> Handler TypedContent
getVCardR i = do
  maybeAddress <- runDB $ selectFirst [ AddressId ==. (toSqlKey i) ] []
  case maybeAddress of
    Just a -> do
      addHeader "Content-Disposition" $ "attachment; filename=\"newcontact.vcf\""
      result <- liftIO $ do
        (hout1, herr1, phan1, x) <- convertJSONtoVCF (DBSL.toStrict (A.encode (addressToXAddr (entityVal a))))
        hClose hout1
        hClose herr1
        return x
      sendResponse (vCardContentType, toContent result)
    Nothing -> return $ TypedContent "" (ContentBuilder "not authorized" Nothing)

data XAddr = XAddr {
  last :: Text
  ,first :: Maybe Text
  ,email :: Maybe [Text]
  ,workphone :: Maybe [Text]
  ,homephone :: Maybe [Text]
  ,cellphone :: Maybe [Text]
  ,birthday :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON XAddr

addressToXAddr :: Address -> XAddr
addressToXAddr (Address lastname firstname datecollected email workphone homephone fax cellphone homeaddress workaddress website tags birthday notes) =
  let ftel u = onlyTels (telSplit u) in
  let feml u = onlyEmls (emlSplit u) in
  XAddr {
    last = lastname,
    first = firstname,
    email = fmap feml email,
    workphone = fmap ftel workphone,
    homephone = fmap ftel homephone,
    cellphone = fmap ftel cellphone,
    birthday = birthday
    } 
