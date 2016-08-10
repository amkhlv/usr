module Handler.NewJSON where

import           Import
import qualified Data.Aeson as A
import qualified Data.Vector as V
import           System.Process

getNewJSONR :: Handler Html
getNewJSONR = do
  (widget, enctype) <- generateFormPost uploadJSONForm
  (widgetVCF, enctypeVCF) <- generateFormPost uploadVCFForm
  defaultLayout $ do
    setTitle "Insert JSON or VCF"
    $(widgetFile "newjson")

postNewJSONR :: Handler Html
postNewJSONR = do
  ((result, widget), enctype) <- runFormPost uploadJSONForm
  case result of
    FormSuccess fileinfo -> do
      topjson <- sourceToList (fileSource fileinfo)
      let objects = case (A.decode (fromChunks topjson) :: Maybe A.Value) of
            Just (A.Array v) -> V.toList v
            _ -> []
      timestamp <- liftIO $ fmap (formatTime defaultTimeLocale "%F") getCurrentTime
      sequence_ $ map (\x -> case x of
                          A.Object o -> do 
                            _ <- runDB (insert (objToAddress o (pack timestamp)))
                            return ()
                          _ -> liftIO $ putStrLn "ERROR: value is not a JSON object"
                      ) objects
      let jsn = unlines (map decodeUtf8 topjson)
      liftIO $ do
        putStrLn "--- START DEBUG ---"
        putStrLn jsn
        putStrLn $ case (parse topjson) of
          Just t -> t
          Nothing -> "======= could not parse ======="
        putStrLn "---  END DEBUG  ---"
      defaultLayout [whamlet|<p>TODO|]
    _ -> defaultLayout
          [whamlet|<p>Something wrong, no file upload|]


convertVCFtoJSON :: [ByteString] -> IO (Handle, Handle, ProcessHandle, [A.Value])
convertVCFtoJSON vcf = do
  (Just hin, Just hout, Just herr, procHandle) <- createProcess  (
    proc
      "/bin/bash"
      ["-c", "/usr/local/lib/amkhlv/vcard2dict.py"]){
    std_in = CreatePipe,
    std_out = CreatePipe,
    std_err = CreatePipe}
  sequence_ (map (hPutStrLn hin) vcf)
  hClose hin
  x <- hGetContents hout
  let objs = case (A.decode x :: Maybe A.Value) of
        Just (A.Array v) -> V.toList v
        _ -> []
  return (hout, herr, procHandle, objs)

postNewVCFR :: Handler Html
postNewVCFR = do
  ((result, widget), enctype) <- runFormPost uploadVCFForm
  case result of
    FormSuccess fileinfo -> do
      vcfs <- sourceToList (fileSource fileinfo)
      (hout, herr, phan, objs) <- liftIO $ convertVCFtoJSON vcfs
      timestamp <- liftIO $ fmap (formatTime defaultTimeLocale "%F") getCurrentTime
      sequence_ $ map (\x -> case x of
                          A.Object o -> do 
                            _ <- runDB (insert (objToAddress o (pack timestamp)))
                            return ()
                          _ -> liftIO $ putStrLn "ERROR: value is not a JSON object"
                      ) objs
      liftIO $ do
        hClose hout
        hClose herr
      defaultLayout [whamlet|<p>TODO|]
    _ -> defaultLayout
          [whamlet|<p>Something wrong, no file upload|]

uploadJSONForm :: Html -> MForm Handler (FormResult FileInfo, Widget)
uploadJSONForm = renderDivs $ fileAFormReq "JSON file"

uploadVCFForm :: Html -> MForm Handler (FormResult FileInfo, Widget)
uploadVCFForm = renderDivs $ fileAFormReq "VCF file"

concatValues :: Text -> Maybe A.Value -> Maybe Text
concatValues separator mv = case mv of
  Just (A.Array xs) -> Just $ intercalate separator (map (\x -> case x of
                                                             A.String y -> y
                                                             _ -> "")
                                                     (V.toList xs))
  _ -> Nothing

idEntityAddress :: Entity Address -> Entity Address
idEntityAddress ea = ea


objToAddress :: A.Object -> Text -> Address
objToAddress a timestamp =
  let lastname = case (lookup "last" a) of
        Just (A.String r) -> r
        _ -> "?"
      firstname = case (lookup "first" a) of
        Just (A.String r) -> Just r
        _ -> Nothing
      datecollected = timestamp
      email = concatValues " , " (lookup "emails" a) 
      workphone = concatValues " , " (lookup "workphones" a) 
      homephone = concatValues " , " (lookup "homephones" a)
      fax = concatValues " , " (lookup "faxes" a)
      cellphone = concatValues " , " (lookup "cellphones" a)
      homeaddress = concatValues "\n===================\n" (lookup "homeaddresses" a)
      workaddress = concatValues "\n===================\n" (lookup "workaddresses" a)
      website = Nothing
      tags = Nothing
      birthday = case (lookup "birthday" a) of
        Just (A.String r) -> Just r
        _ -> Nothing
      orgs = case (lookup "organizations" a) of
        Just (A.Array v) -> Just (intercalate
                                  "\n===================\n"
                                  (map (\x -> case (concatValues "\n" (Just x)) of
                                           Just txt -> txt
                                           Nothing -> ""
                                       ) (V.toList v)))
        _ -> Nothing
      titles = concatValues ", " (lookup "titles" a)
      notes = case titles of
        Just ttls -> case orgs of
          Just os -> Just $ intercalate "\n" [ttls, os]
          Nothing -> Just ttls
        Nothing -> orgs
  in
  Address lastname firstname datecollected email workphone homephone fax cellphone homeaddress workaddress website tags birthday notes


parse :: [ByteString] -> Maybe Text
parse t = case (A.decode (fromChunks t) :: Maybe A.Value) of
  Just (A.Array v) ->
    let tops = V.toList v in
    case tops of
      [] -> Just "====== not found ======"
      x:xs -> case x of
        A.Object h -> case (lookup "last" h) of
          Just (A.String r) -> Just r
          _ -> Just "====== no last name! ======"
        _ -> Just "====== something is wrong ======"
  _ -> Nothing

