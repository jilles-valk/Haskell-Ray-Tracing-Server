module RenderSpec where
    import Test.Hspec
    import Render
    
    spec :: Spec
    spec = do
        describe "render" $ do
            it "render nothing" $
                render ""
                `shouldBe` 
                [] 
            it "render something" $
                length (render "1") > 1
                `shouldBe` 
                True
            it "render scene" $
                render "1"
                `shouldBe` 
                [[0, 1, 0]]