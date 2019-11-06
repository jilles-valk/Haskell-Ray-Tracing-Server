module RenderSpec where
    import Test.Hspec
    import Render
    
    spec :: Spec
    spec = do
        describe "render" $ do
            it "move 1 2 3 by 1 2 3" $
                1
                `shouldBe` 
                0