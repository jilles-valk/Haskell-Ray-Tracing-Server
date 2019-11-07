module Render 
(
    render
)
where 
    import Shapes
    import Data.Colour 
    import View

    render inputString 
        | inputString == [] = [[]]
        | otherwise         = image 
        where 
            sphere = Sphere (Point 0 0 0) 1
            hPixels = 3
            vPixels = 3
            view = View (Point 0 0 3) (Vector 0 0 (-1)) (Vector 0 1 0) hPixels vPixels (0.5*pi) 1.0
            lines = generateLines view
            image = [[colorAtPixel x y (lines !! (fromInteger x) !! (fromInteger y)) sphere| 
                x <- [0.. (hPixels - 1)]] | y <- [0.. (vPixels - 1)]]

    colorAtPixel :: Integer -> Integer -> Line -> Shape -> Integer
    colorAtPixel x y l s 
        | intersect == [] = 0
        | otherwise = colour
        where 
            intersect = intersections l s
            colour = 100;



