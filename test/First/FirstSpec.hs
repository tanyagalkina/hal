module First.FirstSpec (spec) where

import Test.Hspec
import Repl (
    justTest
    ,repl
    ,evalLispExpression)

evalString = "this is not very simply string"


spec :: Spec
spec = do
  describe "just first test using Hspec!" $ do
    it "generates a list from a single Int" $ do
      justTest 8 `shouldBe` [8, 8]

     


 