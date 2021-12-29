module First.FirstSpec (spec) where

import Control.Applicative
import Lexer
import Types
import SchEval
import Test.Hspec
import Repl (
    justTest
    ,repl
    )

evalString = "this is not very simply string"


spec :: Spec
spec = do
  describe "just first test using Hspec!" $ do
    it "generates a list from a single Int" $ do
      justTest 8 `shouldBe` [8, 8]
  describe "parse spaces" $ do    
    it "should parse spaces before the token" $ do
      parse spaces "       Tanya" `shouldBe` [("", "Tanya")]
  describe "parse string Chorowo!" $ do    
    it "should look if Chorowo! is there (could have leading spaces)" $ do
      parse checkString "   Chorowo!" `shouldBe` [("Chorowo!", "")]

  describe "parse string Chorowo!" $ do    
    it "should look if Chorowo! is there (could have leading spaces)" $ do
      parse checkString "Chorowo!" `shouldBe` [("Chorowo!", "")]
  

  describe "atleastonespace" $ do    
    it "parsing at least one space" $ do
      parse atleastoneSpace " ma" `shouldBe` [(' ', "ma")]

  describe "atleastonespace" $ do    
    it "parsing at least one space fail" $ do
      parse atleastoneSpace "mamapapa" `shouldBe` []

  describe "TOKEN" $ do    
    it "parsing at least one space fail" $ do
      parse token "eq?    " `shouldBe` [(Var "eq", "    ")]
    


  describe "EQ" $ do    
    it "parsing symbol eq?" $ do
      parse eq "eq? " `shouldBe` [("eq?", " ")]

  describe "LIST1" $ do    
    it "parsing symbol eq?" $ do
      parse list "(+  8 9)" `shouldBe` [(Var "+", "  ")]    
    

  describe "symb" $ do    
    it "parsing symbol eq?" $ do
      parse symb "*  " `shouldBe` [(Var "*", "  ")]    
      
    

  -- describe "checking dottedPair" $ do    
  --   it "recognizing that there is something with ' . ' in between" $ do
  --     parse dottedPair "(mama . papa)" `shouldBe` [("(mama . papa)", "")]

  -- describe "checking some alphanum" $ do    
  --   it "what does it exactly do?" $ do
  --     parse token "      mama123 " `shouldBe` [("mama123", " ")]         
       
  -- describe "checking dot separately" $ do    
  --   it "parsing dot in the pair" $ do
  --    parse dot "      . " `shouldBe` [(" . ", "")]

  -- describe "looking for float" $ do    
  --   it "should take all th numbers" $ do
  --    parse tokenFlt "     8 9 10" `shouldBe` [(8, " 9 10")]

  describe "parsing to expr types" $ do    
    it "returns a SchExpr :: Float" $ do
     parse tokenFlt "     8 " `shouldBe` [((Float 8), " ")]

  describe "parsing to expr types 2" $ do    
    it "returns a SchExpr Bool False" $ do
     parse boolean "     #f " `shouldBe` [((Bool False), " ")]

  describe "parsing to expr types 2" $ do    
    it "returns a SchExpr Var" $ do 
     parse token "   name  " `shouldBe` [((Var "name"), "  ")]

  describe "parsing to expr types 2" $ do    
    it "returns a SchExpr Var" $ do 
     parse list "(+   name (+ haah ) 9 87 5 )" `shouldBe` [((Var "name"), "  ")]   

                  
              
              
        
      

     


 