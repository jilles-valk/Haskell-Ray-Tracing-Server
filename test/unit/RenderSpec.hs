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
                (isJust $ fst (getNearestIntersectingObject 
                line []))
                `shouldBe`
                False  
            it "just one" $
                (fromJust $ fst $ getNearestIntersectingObject  
                (Line (Point 0 0 0) (Vector 1 2 3)) [Sphere (Point 1 2 3) 0.5])
                `shouldBe`
                (Sphere (Point 1 2 3) 0.5)
            it "just two" $
                (fromJust $ fst $ getNearestIntersectingObject 
                (Line (Point 0 0 0) (Vector 1 1 1)) [Sphere (Point 2 2 2) 0.5, Sphere (Point 1 1 1) 0.5])
                `shouldBe`
                (Sphere (Point 1 1 1) 0.5)
            it "just two one intersect" $
                (fromJust $ fst $ getNearestIntersectingObject 
                (Line (Point 0 0 0) (Vector 1 1 1)) [Sphere (Point 2 2 2) 0.5, Sphere (Point (-10) 1 1) 0.5])
                `shouldBe`
                (Sphere (Point 2 2 2) 0.5)
            it "just two none intersect" $
                (isNothing $ fst $ getNearestIntersectingObject 
                (Line (Point 0 0 0) (Vector 0 0  1)) [Sphere (Point 2 2 2) 0.5, Sphere (Point (-10) 1 1) 0.5])
                `shouldBe`
                True
            it "quite a few" $
                (fromJust $ fst $ getNearestIntersectingObject 
                (Line (Point 0 0 0) (Vector 1 1 1)) 
                [Sphere (Point 2 2 2) 0.1, Sphere (Point (-10) 1 1) 0.1, Sphere (Point 1 1 1) 0.1, 
                Sphere (Point 0.3 0.3 0.3) 0.1, Sphere (Point 0.5 0.5 0.5) 0.1, Sphere (Point (-9) 2 2) 0.1])
                `shouldBe`
                (Sphere (Point 0.3 0.3 0.3) 0.1)
        describe "checkBlocked" $ do
            it "no objects" $
                (checkBlocked (Line (Point 0 0 (5)) (Vector 0 0 (-1))) 
                    []) 
                `shouldBe`
                False
            it "one object same" $
                (checkBlocked (Line (Point 0 0 (1)) (Vector 0 0 (-1))) 
                    [(Sphere (Point 0 0 0) 1)]) 
                `shouldBe`
                True
            it "one object other" $
                (checkBlocked (Line (Point 0 0 (5)) (Vector 0 0 (-1))) 
                    [(Sphere (Point 0 0 0) 1)]) 
                `shouldBe`
                True
            it "two objects" $
                (checkBlocked (Line (Point 0 0 (5)) (Vector 0 0 (-1))) 
                    [(Sphere (Point 0 0 0) 1), (Sphere (Point 0 0 0) 1)]) 
                `shouldBe`
                True