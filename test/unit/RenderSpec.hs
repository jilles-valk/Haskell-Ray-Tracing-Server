module RenderSpec where
    import Test.Hspec
    import Data.Maybe
    import Render
    import Shapes
    
    spec :: Spec
    spec = do
        let line = (Line (Point 0 0 0) (Vector 0 0 (-1)))
        -- let pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128 
        -- describe "render" $ do
        --     it "render nothing" $
        --         render ""
        --         `shouldBe` 
        --         generateImage pixelRenderer 250 300
        --     it "render something" $
        --         length (render "1") > 1
        --         `shouldBe` 
        --         True
        --     it "render scene" $
        --         render "1"
        --         `shouldBe` 
        --         generateImage pixelRenderer 250 300
        describe "nearest object" $ do
            it "just zero" $
                (isNothing $ (getNearestIntersectingObject 
                line []))
                `shouldBe`
                True  
            it "just one" $
                (fromJust $ getNearestIntersectingObject  
                (Line (Point 0 0 0) (Vector 1 2 3)) [Sphere (Point 1 2 3) 0.5])
                `shouldBe`
                (Sphere (Point 1 2 3) 0.5)
            it "just two" $
                (fromJust $ getNearestIntersectingObject 
                (Line (Point 0 0 0) (Vector 1 1 1)) [Sphere (Point 2 2 2) 0.5, Sphere (Point 1 1 1) 0.5])
                `shouldBe`
                (Sphere (Point 1 1 1) 0.5)
            it "just two one intersect" $
                (fromJust $ getNearestIntersectingObject 
                (Line (Point 0 0 0) (Vector 1 1 1)) [Sphere (Point 2 2 2) 0.5, Sphere (Point (-10) 1 1) 0.5])
                `shouldBe`
                (Sphere (Point 2 2 2) 0.5)
            it "just two none intersect" $
                (isNothing $ getNearestIntersectingObject 
                (Line (Point 0 0 0) (Vector 0 0  1)) [Sphere (Point 2 2 2) 0.5, Sphere (Point (-10) 1 1) 0.5])
                `shouldBe`
                True
            it "quite a few" $
                (fromJust $ getNearestIntersectingObject 
                (Line (Point 0 0 0) (Vector 1 1 1)) 
                [Sphere (Point 2 2 2) 0.1, Sphere (Point (-10) 1 1) 0.1, Sphere (Point 1 1 1) 0.1, 
                Sphere (Point 0.3 0.3 0.3) 0.1, Sphere (Point 0.5 0.5 0.5) 0.1, Sphere (Point (-9) 2 2) 0.1])
                `shouldBe`
                (Sphere (Point 0.3 0.3 0.3) 0.1)
