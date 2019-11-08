module Render 
(
    render
)
where 
    import Shapes
    import Data.Colour 
    import View
    import Codec.Picture

    render inputString 
        | inputString == [] = writePng "img.png" image
        | otherwise         = writePng "img.png" image
        where 
            sphere = Sphere (Point 0 0 0) 1
            hPixels = 30
            vPixels = 30
            view = View (Point 0 0 3) (Vector 0 0 (-1)) (Vector 0 1 0) hPixels vPixels (0.5*pi) 1.0
            lines = generateLines view
            -- image = [[colorAtPixel x y (lines !! (fromInteger x) !! (fromInteger y)) sphere| 
            --     x <- [0.. (hPixels - 1)]] | y <- [0.. (vPixels - 1)]]

            image = generateImage (\x y -> generatePixel ( x) ( y) lines sphere) (fromInteger vPixels) (fromInteger hPixels)

    -- colorAtPixel :: Integer -> Integer -> Line -> Shape -> Integer
    colorAtPixel l s 
        | intersect == [] = 0
        | otherwise = colour
        where 
            intersect = intersections l s
            colour = 100;

    -- generatePixel :: Int -> Int -> [[Line]] -> Shape -> PixelRGB8
    generatePixel x y lines sphere = PixelRGB8 
        ( (colorAtPixel (lines !! x !! y) sphere)) 
        ( (colorAtPixel (lines !! x !! y) sphere)) 128



