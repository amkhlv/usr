import           System.Console.Haskeline
import           Crypto.PasswordStore
import           System.IO
import           Control.Exception
import           Data.ByteString.Char8 
import           Data.Maybe (fromJust)

main :: IO ()
main = do
  x <- runInputT defaultSettings $ do
    getPassword (Just '*') "Enter password to hash: "
  h <- makePassword (pack (fromJust x)) 18
  System.IO.putStrLn $ (unpack h)





