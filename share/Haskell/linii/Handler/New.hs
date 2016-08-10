module Handler.New where

import Import

getNewR :: Handler Html
getNewR = do
    (newWidget, newEnctype) <- generateFormPost newForm
    defaultLayout $ do
        setTitle "NewAbkEntry"
        $(widgetFile "new")

postNewR :: Handler Html
postNewR = do
    ((newResult, newWidget), newEnctype) <- runFormPost newForm
    case newResult of
      FormSuccess res ->
        do
          newid <- runDB $ insert (a1toAddress res)
          t <- liftIO $ fmap (formatTime defaultTimeLocale "%F") getCurrentTime
          runDB $ update newid [AddressDatecollected =. (pack t)]
          getNewR
      _ -> getNewR

  
newForm :: Html -> MForm Handler (FormResult A1, Widget)
newForm = renderDivs $ A1
  <$> areq textField "Last Name" Nothing
  <*> aopt textField "First Name" Nothing
  <*> areq hiddenField "" (Just "--")
  <*> aopt textareaField "email" Nothing
  <*> aopt textareaField "workphone" Nothing
  <*> aopt textareaField "homephone" Nothing
  <*> aopt textareaField "fax" Nothing
  <*> aopt textareaField "cellphone" Nothing
  <*> aopt textareaField "homeaddress" Nothing
  <*> aopt textareaField "workaddress" Nothing
  <*> aopt textareaField "website" Nothing
  <*> aopt textareaField "tags" Nothing
  <*> aopt textareaField "birthday" Nothing
  <*> aopt textareaField "notes" Nothing


a1toAddress :: A1 -> Address
a1toAddress a = Address
  (a1NewLast a)
  (a1NewFirst a)
  (a1NewDatecollected a)
  (fmap unTextarea $ a1NewEmail a)
  (fmap unTextarea $ a1NewWorkphone a)
  (fmap unTextarea $ a1NewHomephone a)
  (fmap unTextarea $ a1NewFax a)
  (fmap unTextarea $ a1NewCellphone a)
  (fmap unTextarea $ a1NewHomeaddress a)
  (fmap unTextarea $ a1NewWorkaddress a)
  (fmap unTextarea $ a1NewWebsite a)
  (fmap unTextarea $ a1NewTags a)
  (fmap unTextarea $ a1NewBirthday a)
  (fmap unTextarea $ a1NewNotes a)
