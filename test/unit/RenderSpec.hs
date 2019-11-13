module RenderSpec where
    import Test.Hspec
    import Data.Maybe
    import qualified Data.ByteString.Lazy as BL
    import Render
    import Shapes
    import View
    
    spec :: Spec
    spec = do
        let line = (Line (Point 0 0 0) (Vector 0 0 (-1)))
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
        describe "parseJSON" $ do
            it "get view" $
                (parseObjects $ BL.pack "test") --"[{\"z\":3,\"x\":1,\"y\":2},{\"zv\":2,\"yv\":2,\"xv\":2},{\"zv\":3,\"yv\":3,\"xv\":3},4,5,1,2]")
                `shouldBe`
                (View (Point 1 2 3) (Vector 2 2 2) (Vector 3 3 3) 4 5 1.0 2.0)
