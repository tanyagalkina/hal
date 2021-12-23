module First.SecondSpec (spec) where

import Test.Hspec
import Repl (
    justTest
    ,repl
    ,evalLispExpression)
import Errors (zeroto)    

evalString = "this is not very simply string"


spec :: Spec
spec = do
  describe "just first test using Hspec!" $ do
    it "generates a list from a single Int" $ do
      justTest 8 `shouldBe` [8, 8]
  describe "testing zeroto function" $ do    
    it "returns Int array from zero to n" $ do
      zeroto 8 `shouldBe` [0, 1, 2, 3, 4, 5, 6, 7, 8]      