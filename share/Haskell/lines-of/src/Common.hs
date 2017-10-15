module Common where

import           Control.Applicative
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToRow
import qualified Data.Text as T
import qualified Data.List as L

data Injection = Injection {
  rowspecs    :: [(String, (Int, Int))]
  , csspath   :: String
  , dbpath    :: String
  , table     :: String
  , insert    :: [String] -> IO ()
  , selectAll :: IO [[String]]
  , select    :: String -> IO [[String]]
  , delete    :: [String] -> IO ()
  }

type RowSpecs = [(String, (Int, Int))]
type CSSPath = String
type DBPath = String
type TableName = String

inject :: CSSPath -> DBPath -> TableName -> RowSpecs -> Injection
inject cssfilename filename tablename rconfs =
  let qmarks = "(" ++ L.intersperse ',' ['?'| r <- rconfs] ++ ")"
      cscols = "(" ++ L.intercalate "," [fst r | r <- rconfs] ++ ")"
      matchs = L.intercalate " and " [ fst r ++ "=?" | r <- rconfs]
      insertInj xs = do
        conn <- open filename
        withTransaction conn $
          execute conn (Query . T.pack $ "INSERT INTO " ++ tablename ++ " " ++ cscols ++ " VALUES " ++ qmarks) xs
        close conn
      selectAllInj = do
        conn <- open filename
        rs <- query_ conn (Query . T.pack $ "SELECT * FROM " ++ tablename) :: IO [[String]]
        close conn
        return rs
      selectInj q = do
        conn <- open filename
        rs <- query_ conn (Query . T.pack $ "SELECT * FROM " ++ tablename ++ " WHERE " ++ q) :: IO [[String]]
        close conn
        return rs
      deleteInj xs = do
        conn <- open filename
        withTransaction conn $
          execute conn (Query . T.pack $ "DELETE FROM " ++ tablename ++ " WHERE " ++ matchs) xs
        close conn
  in Injection rconfs cssfilename filename tablename insertInj selectAllInj selectInj deleteInj
