module ShapesSpec where

    import Test.Hspec
    import Shapes
    
    spec :: Spec
    spec = do
        describe "movePoint" $ do
            it "move 1 2 3 by 1 2 3" $
                (Point 2 4 6)  
                `shouldBe` 
                (moveP (Point 1 2 3)  (Vector 1 2 3))

        describe "toUnitVector" $ do
            it "Creating vector 4 2 4 should get unit" $
                (Vector (2/3) (1/3) (2/3))
                `shouldBe` 
                toUnitVector (Vector 4 2 4)

        describe "addVectors" $ do
            it "Add 5 5 5 and -1 -1 -1" $
                (Vector 4 4 4) 
                `shouldBe` 
                ((Vector 5 5 5) `addV` (Vector (-1) (-1) (-1)))

        describe "subtractV" $ do
            it "Subtract 1 2 3 from 5 5 5" $
                (Vector 4 3 2) 
                `shouldBe` 
                ((Vector 5 5 5) `subtractV` (Vector 1 2 3))
        
        describe "moveS" $ do
            it "Move 1 2 3 by 1 2 3" $
                (Sphere (Point 2 4 6) 1) 
                `shouldBe` 
                (moveS (Sphere (Point 1 2 3) 1)  (Vector 1 2 3))

        describe "getPointOnLine" $ do
            it "Get point at 1 from 0" $
                (Point 1 1 1)
                `shouldBe` 
                (getPointOnLine (Line (Point 0 0 0) (Vector 1 1 1)) 1)

        describe "dot" $ do
            it "Dot of 1 0 0 and 0 1 0" $
                (dot (Vector 1 0 0) (Vector 0 1 0)) 
                `shouldBe`
                (0)

        describe "intersections" $ do
            let sphere = Sphere (Point 0 0 0) 1
            let otherSphere = Sphere (Point 1 0 0) 2
            it "Intersect throught centre" $
                (intersections  (Line (Point 0 0 (-5)) (Vector 0 0 1)) sphere)
                `shouldBe`
                ([4.0, 6.0])
            it "Intersect at radius" $
                (intersections  (Line (Point 1 0 (-5)) (Vector 0 0 1)) sphere)
                `shouldBe`
                ([5.0, 5.0])
            it "Intersect at radius other sphere" $
                (intersections  (Line (Point 3 0 (-5)) (Vector 0 0 1)) otherSphere)
                `shouldBe`
                ([5.0, 5.0])
            it "No intersection" $
                (intersections  (Line (Point 1 3 (-5)) (Vector 0 0 1)) sphere)
                `shouldBe`
                ([])