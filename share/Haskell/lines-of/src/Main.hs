module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.Pickle

import Common
import GUI

data CLOps = CLOps
  { xml :: String
  }

prsr :: Parser CLOps
prsr = CLOps <$> strOption ( short 'x' <> metavar "PATH_TO_XMLCONF" <> help "path to XML config" )

data TableSpecs = TableSpecs
  { csspath   :: String
  , dbpath    :: String
  , table     :: String
  , colspecs  :: [(String, Int, Int)]
  }

instance XmlPickler TableSpecs where xpickle = xpTableSpecs
xpTableSpecs :: PU TableSpecs
xpTableSpecs =
  xpElem "table" $
  xpWrap ( \((c,d,t,r)) -> TableSpecs c d t r , \s -> (Main.csspath s, Main.dbpath s, Main.table s, Main.colspecs s) ) $
  xp4Tuple
  (xpElem "cssfile" xpText)
  (xpElem "dbfile" xpText)
  (xpElem "tablename" xpText)
  (xpElem
   "columns"
   (xpList
    (xpElem
     "column"
     (xpTriple
      (xpAttr "name" xpText)
      (xpAttr "width" xpInt)
      (xpAttr "nlines" xpInt)))))

getSpecs :: String -> IO [Maybe TableSpecs]
getSpecs xmlfile = runX $ readDocument [withValidate no] xmlfile
  >>>
  getChildren
  >>>
  (processChildren $ ifA (hasName "columns") (processChildren isElem) isElem)
  >>^
  (unpickleDoc xpTableSpecs)

main :: IO()
main = do
  CLOps xmlfile <- execParser $ info (prsr <**> helper) (fullDesc <> progDesc "SQLite interaction")
  mts <- getSpecs xmlfile
  case mts of
    [Just ts] ->
      starter $ inject (Main.csspath ts) (Main.dbpath ts) (Main.table ts) [ (nm, (wd, nl)) | (nm, wd, nl) <- colspecs ts ]
    _ ->
      error "error reading XML" 
