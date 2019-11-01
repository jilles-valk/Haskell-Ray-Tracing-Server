module Main where

    import Test.Tasty
    import Test.Tasty.HUnit
    import Shapes
    
    main :: IO ()
    main = do
      defaultMain (testGroup "Our Shapes Tests" 
        [movePointTest, addVectorsTest, subtractVectorTest, moveSphereTest])
    
    movePointTest :: TestTree
    movePointTest = testCase "Testing movePointTest"
        (assertEqual "Move 1 2 3 by 1 2 3"  
            (Point 2 4 6) 
            (moveP (Point 1 2 3)  (Vector 1 2 3)))
    
    addVectorsTest :: TestTree
    addVectorsTest = testCase "Testing addVectorsTest"
        (assertEqual "Add 5 5 5 and -1 -1 -1" 
            (Vector 4 4 4) 
            ((Vector 5 5 5) `addV` (Vector (-1) (-1) (-1))))

    subtractVectorTest :: TestTree
    subtractVectorTest = testCase "Testing subtractVectorTest"
        (assertEqual "Subtract 1 2 3 from 5 5 5" 
            (Vector 4 3 2) 
            ((Vector 5 5 5) `subtractV` (Vector 1 2 3)))
    
    moveSphereTest :: TestTree
    moveSphereTest = testCase "Testing moveSphereTest"
        (assertEqual "Move 1 2 3 by 1 2 3"  
            (Sphere (Point 2 4 6) 1) 
            (moveS (Sphere (Point 1 2 3) 1)  (Vector 1 2 3)))