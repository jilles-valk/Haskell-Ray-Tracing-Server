module ViewSpec where

    import Test.Hspec
    import View
    import Shapes
    
    spec :: Spec
    spec = do
        describe "make view" $ do
            let view32 = View (Point 0 0 0) (Vector 0 0 (-1)) (Vector 0 1 0) 3 2 (0.5*pi) 1.0
            let view33 = View (Point 0 0 0) (Vector 0 0 (-1)) (Vector 0 1 0) 3 3 (0.5*pi) 1.0
            it "returns a line through the top left corner of the view" $
               (generateLines view33 !! 0 !! 0)
               `shouldBe`
               (Line (Point 0.0 0.0 1.0) (Vector (-0.5773501) 0.5773505 (-0.57735026)))
            it "returns a line through the top right corner of the view" $
               (generateLines view33 !! 0 !! 2)
               `shouldBe`
               (Line (Point 0.0 0.0 1.0) (Vector 0.57735044 0.5773502 (-0.57735026)))
            it "returns a line through the bottom left corner of the view" $
               (generateLines view33 !! 2 !! 0)
               `shouldBe`
               (Line (Point 0.0 0.0 1.0) (Vector (-0.5773502) (-0.5773502) (-0.5773504)))
            it "returns a line through the bottom right corner of the view" $
               (generateLines view33 !! 2 !! 2)
               `shouldBe`
               (Line (Point 0.0 0.0 1.0) (Vector 0.5773504 (-0.5773502) (-0.5773502)))
            it "returns a line through the top middle of the view" $
               (generateLines view33 !! 0 !! 1)
               `shouldBe`
               (Line (Point 0.0 0.0 1.0) (Vector 2.5288108e-7 0.7071068 (-0.7071067)))
            it "returns a line through the middle of the view" $
               (generateLines view33 !! 1 !! 1)
               `shouldBe`
               (Line (Point 0.0 0.0 1.0) (Vector 3.5762787e-7 1.4901156e-7 (-1.0)))
    
        