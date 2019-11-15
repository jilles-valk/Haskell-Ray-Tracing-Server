module ViewSpec where

    import Test.Hspec
    import View
    import Shapes
    
    spec :: Spec
    spec = do
         let view33 = View (Point 0 0 0) (Vector 0 0 (-1)) (Vector 0 1 0) 3 3 (0.5*pi)
         describe "test square viewTemp" $ do   
            it "returns a line through the top left corner of the view" $
               (generateLines view33 !! 0)
               `shouldBe`
               (Line (Point 0.0 0.0 1.0) (Vector (-0.5773501) 0.5773505 (-0.57735026)))
            it "returns a line through the top middle of the view" $
               (generateLines view33 !! 1)
               `shouldBe`
               (Line (Point 0.0 0.0 1.0) (Vector 2.5288108e-7 0.7071068 (-0.7071067)))
            it "returns a line through the top right corner of the view" $
               (generateLines view33 !! 2)
               `shouldBe`
               (Line (Point 0.0 0.0 1.0) (Vector 0.57735044 0.5773502 (-0.57735026)))
            it "returns a line through the middle left of the view" $
               (generateLines view33 !! 3)
               `shouldBe`
               (Line (Point 0.0 0.0 1.0) (Vector (-0.70710665) 2.5288114e-7 (-0.7071069)))
            it "returns a line through the middle of the view" $
               (generateLines view33 !! 4)
               `shouldBe`
               (Line (Point 0.0 0.0 1.0) (Vector 3.5762787e-7 1.4901156e-7 (-1.0)))
            it "returns a line through the middle right of the view" $
               (generateLines view33 !! 5)
               `shouldBe`
               (Line (Point 0.0 0.0 1.0) (Vector 0.70710695 (-4.2146922e-8) (-0.70710665)))
            it "returns a line through the bottom left corner of the view" $
               (generateLines view33 !! 6)
               `shouldBe`
               (Line (Point 0.0 0.0 1.0) (Vector (-0.5773502) (-0.5773502) (-0.5773504)))
            it "returns a line through the bottom middle of the view" $
               (generateLines view33 !! 7)
               `shouldBe`
               (Line (Point 0.0 0.0 1.0) (Vector 2.528811e-7 (-0.7071067) (-0.7071068)))
            it "returns a line through the bottom right corner of the view" $
               (generateLines view33 !! 8)
               `shouldBe`
               (Line (Point 0.0 0.0 1.0) (Vector 0.5773504 (-0.5773502) (-0.5773502)))
         describe "test square view" $ do
            let view32 = View (Point 0 0 0) (Vector 0 0 (-1)) (Vector 0 1 0) 3 2 (0.5*pi)
            it "returns a line through the top left corner of the view" $
               (generateLines view32 !! 0)
               `shouldBe`
               (Line (Point 0.0 0.0 1.0) (Vector (-0.5773501) 0.5773505 (-0.57735026)))
            it "returns a line through the top middle of the view" $
               (generateLines view32 !! 1)
               `shouldBe`
               (Line (Point 0.0 0.0 1.0) (Vector 2.5288108e-7 0.7071068 (-0.7071067)))
            it "returns a line through the top right corner of the view" $
               (generateLines view32 !! 2)
               `shouldBe`
               (Line (Point 0.0 0.0 1.0) (Vector 0.57735044 0.5773502 (-0.57735026)))
            it "returns a line through the middle left of the view" $
               (generateLines view32 !! 3)
               `shouldBe`
               (Line (Point 0.0 0.0 1.0) (Vector (-0.70710665) 2.5288114e-7 (-0.7071069)))
         describe "test generateLinesHelper" $ do
            let startLines = [Line (Point 0 0 5) (Vector (0.19611613) 0 (-0.9805807))]
            it "one hor pixel" $
               ((generateLinesHelper (Point 0 0 5) (Vector (-1) 0 0) (Vector 0 1 0) 
                  (Point 1 0 0) (Point 1 0 0) 1 1 1 ([Line (Point 0 0 5) (Vector (0.5) 0 (0.5))])) !! 0)
               `shouldBe`
               (Line (Point 0 0 5) (Vector (0.5) 0 (0.5)))
            it "three hor pixels" $
               ((generateLinesHelper (Point 0 0 5) (Vector (-1) 0 0) (Vector 0 1 0) 
                  (Point 1 0 0) (Point 1 0 0) 3 1 1 (startLines)) !! 0)
               `shouldBe`
               (Line (Point 0 0 5) (Vector (-0.19611613) 0 (-0.9805807)))
            it "three hor pixels second pixel" $
               ((generateLinesHelper (Point 0 0 5) (Vector (-1) 0 0) (Vector 0 1 0) 
                  (Point 1 0 0) (Point 1 0 0) 3 1 1 (startLines)) !! 1)
               `shouldBe`
               (Line (Point 0 0 5) (Vector (0.0) 0.0 (-1.0)))
            it "three hor pixels third pixel" $
               ((generateLinesHelper (Point 0 0 5) (Vector (-1) 0 0) (Vector 0 1 0) 
                  (Point 1 0 0) (Point 1 0 0) 3 1 1 (startLines)) !! 2)
               `shouldBe`
               (Line (Point 0 0 5) (Vector (0.19611613) 0 (-0.9805807)))
            it "three hor 2 ver pixels top right pixel" $
               ((generateLinesHelper (Point 0 0 5) (Vector (-1) 0 0) (Vector 0 1 0) 
                  (Point 1 0 0) (Point 1 0 0) 3 2 1 (startLines)) !! 0)
               `shouldBe`
               (Line (Point 0 0 5) (Vector (-0.19245009) (0.19245009) (-0.9622505)))
            it "three hor 2 ver pixels top middle pixel" $
               ((generateLinesHelper (Point 0 0 5) (Vector (-1) 0 0) (Vector 0 1 0) 
                  (Point 1 0 0) (Point 1 0 0) 3 2 1 (startLines)) !! 1)
               `shouldBe`
               (Line (Point 0 0 5) (Vector 0 (0.19611613) (-0.9805807)))
            it "three hor 2 ver pixels top left pixel" $
               ((generateLinesHelper (Point 0 0 5) (Vector (-1) 0 0) (Vector 0 1 0) 
                  (Point 1 0 0) (Point 1 0 0) 3 2 1 (startLines)) !! 2)
               `shouldBe`
               (Line (Point 0 0 5) (Vector (0.19245009) (0.19245009) (-0.9622505)))
            it "three hor 2 ver pixels bottom left pixel" $
               ((generateLinesHelper (Point 0 0 5) (Vector (-1) 0 0) (Vector 0 1 0) 
                  (Point 1 0 0) (Point 1 0 0) 3 2 1 (startLines)) !! 3)
               `shouldBe`
               (Line (Point 0 0 5) (Vector (-0.19611613) 0 (-0.9805807)))
            
    
        