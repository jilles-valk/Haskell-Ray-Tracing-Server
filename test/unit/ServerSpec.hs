module ShapesSpec where

    import Test.Hspec
    import Server
    
    spec :: Spec
    spec = do
        describe "movePoint" $ do
            it "move 1 2 3 by 1 2 3" $
                (Point 2 4 6)  
                `shouldBe` 
                (moveP (Point 1 2 3)  (Vector 1 2 3))