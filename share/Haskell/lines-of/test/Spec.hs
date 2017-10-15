import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad.IO.Class

import Common

injected = inject
  "example.css"
  "test.db"
  "test"
  [  ("nick", (10, 1, False))
  ,  ("name", (20, 3, False))
  -- (colname, (width, nlines))
  ]

main :: IO ()
-- main = putStrLn "Test suite not yet implemented"
main = do
  delete injected ["last", "Last Entry"]
  insert injected ["last", "Last Entry"]
  r <- selectAll injected
  putStrLn ""
  putStrLn $ show r
  hspec $ do
    describe "Prelude.head" $ do
      it "returns the first element of a list" $ do
        head [23 ..] `shouldBe` (23 :: Int)

      it "returns the first element of an *arbitrary* list" $
        property $ \x xs -> head (x:xs) == (x :: Int)

      it "throws an exception if used with an empty list" $ do
        evaluate (head []) `shouldThrow` anyException
    describe "Example" $ do
      it "inserts" $ 
        property (last r == ["last", "Last Entry"])
