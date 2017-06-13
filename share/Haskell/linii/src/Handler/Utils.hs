module Handler.Utils where

import Prelude()
import Import
import Text.Regex.PCRE
import Data.Text (split)
import qualified Data.ByteString.Char8 as C


data TelItem = Tel Text | NoTel Text
data EmlItem = Eml Text | NoEml Text

data Chunk = W Text | Delim Char

strongDelims :: [Char]
strongDelims = [',', '\t', '\r', '\n']

weakDelims :: [Char]
weakDelims = [' ']

splitChuCha :: Chunk -> Char -> [Chunk]
splitChuCha (Delim y) _ = [Delim y]
splitChuCha (W u) c = intersperse (Delim c) (map W (split (== c) u))

isTel :: Text -> Bool
isTel x =
  let y = unpack x :: String in
  let z = C.pack "([a-z]|[A-Z])" :: ByteString in
  not ((y =~ z :: Bool) || x == pack "")

isEml :: Text -> Bool
isEml x = case find (== '@') x of
  Just _ -> True
  Nothing -> False

chunksToTelItems :: [Chunk] -> [TelItem] -> [TelItem]
chunksToTelItems [] acc = reverse acc
chunksToTelItems [W txt] acc = reverse $ (if (isTel txt) then Tel txt else NoTel txt):acc
chunksToTelItems ((W x):(Delim c):(W y):cs) acc =
  case ((isTel x) , (isTel y)) of
    (True, True) -> if (elem c weakDelims) then
                      chunksToTelItems ((W (x ++ pack "-" ++ y)):cs) acc
                    else
                      chunksToTelItems ((W y):cs) ((NoTel $ singleton c):(Tel x):acc)
    (True, False) -> chunksToTelItems ((W y):cs) ((NoTel $ singleton c):(Tel x):acc)
    (False, True) -> chunksToTelItems ((W y):cs) ((NoTel $ x ++ (singleton c)):acc)
    (False, False) -> chunksToTelItems ((W (x ++ (singleton c) ++ y)):cs) acc
chunksToTelItems _ acc = (NoTel $ pack "--PARSING ERROR--"):(reverse acc)

telSplit :: Text -> [TelItem]
telSplit x = chunksToTelItems  (foldM splitChuCha (W x) (strongDelims ++ weakDelims)) []

chunksToEmlItems :: [Chunk] -> [EmlItem] -> [EmlItem]
chunksToEmlItems [] acc = reverse acc
chunksToEmlItems [W txt] acc = reverse $ (if (isEml txt) then Eml txt else NoEml txt):acc
chunksToEmlItems ((W x):(Delim c):(W y):cs) acc =
  case ((isEml x) , (isEml y)) of
    (False, False) -> chunksToEmlItems ((W (x ++ (singleton c) ++ y)):cs) acc
    (b, _) -> chunksToEmlItems ((W y):cs) ((NoEml $ singleton c):(if b then Eml x else NoEml x):acc)
chunksToEmlItems _ acc = (NoEml $ pack "--PARSING ERROR--"):(reverse acc)

emlSplit :: Text -> [EmlItem]
emlSplit x = chunksToEmlItems  (foldM splitChuCha (W x) (strongDelims ++ weakDelims)) []

itemIsTel :: TelItem -> Bool
itemIsTel (Tel _) = True
itemIsTel (NoTel _) = False

unTelItem :: TelItem -> String
unTelItem ti = case ti of
  Tel x -> unpack x
  NoTel x -> unpack x

onlyTels :: [TelItem] -> [Text]
onlyTels ts = foldr (\ti -> \acc -> case ti of { Tel x -> x:acc ; NoTel x -> acc }) [] ts

itemIsEml :: EmlItem -> Bool
itemIsEml (Eml _) = True
itemIsEml (NoEml _) = False

unEmlItem :: EmlItem -> String
unEmlItem ti = case ti of
  Eml x -> unpack x
  NoEml x -> unpack x

onlyEmls :: [EmlItem] -> [Text]
onlyEmls ts = foldr (\ti -> \acc -> case ti of { Eml x -> x:acc ; NoEml x -> acc }) [] ts
