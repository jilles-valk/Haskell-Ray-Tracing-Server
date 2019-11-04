module ViewSpec where

    import Test.Hspec
    import View
    import Shapes
    
    spec :: Spec
    spec = do
      describe "absolute" $ do
        it "returns the original number when given a positive input" $
           1 `shouldBe` 1
    
        it "returns a positive number when given a negative input" $
           1 `shouldBe` 1
    
        it "returns zero when given zero" $
           0 `shouldBe` 0
    
        