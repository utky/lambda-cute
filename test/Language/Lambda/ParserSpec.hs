module Language.Lambda.ParserSpec (spec) where


import           Test.Hspec
import           Test.QuickCheck
import           Language.Lambda.Syntax
import           Language.Lambda.Parser

spec :: Spec
spec = do
  describe "Parser" $ do
    it "can parse variable" $ do
      runLambdaParser "x" `shouldBe` (Right (Var "x"))

    it "can parse variable with paren" $ do
      runLambdaParser "(x)" `shouldBe` (Right (Var "x"))

    it "can parse lambda abstraction" $ do
      runLambdaParser "λx. x" `shouldBe` (Right (Abs "x" (Var "x")))

    it "can parse lambda abstraction with paren" $ do
      runLambdaParser "(λx. x)" `shouldBe` (Right (Abs "x" (Var "x")))

    it "can parse function application" $ do
      runLambdaParser "((λx. x) (λy. y))" `shouldBe`
        (Right (App (Abs "x" (Var "x")) (Abs "y" (Var "y"))))
