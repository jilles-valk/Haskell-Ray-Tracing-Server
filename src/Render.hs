module Render 
(
    render,
    colorAtPixel,
    generatePixel
)
where 
    import Shapes
    import Data.Colour 
    import View
    import Codec.Picture
    import Data.Aeson
    import Data.Aeson.Types

    render inputJSON 
        -- | inputString == _ = image
        -- | otherwise         
        = encodePng $ generateImage 
        (\x y -> generatePixel (lines !! x !! y) sphere) 
        (fromInteger hPixels) (fromInteger vPixels)
        where 
            sphere = parseObjects inputJSON
            hPixels = 300
            vPixels = 200
            view = View (Point 0 0 3) (Vector 0 0 (-1)) (Vector 0 1 0) hPixels vPixels (0.5*pi) (3/2)
            lines = generateLines view
            -- image = encodePng $ generateImage 
            --     (\x y -> generatePixel x y (lines !! x !! y) sphere) 
            --     (fromInteger vPixels) (fromInteger hPixels)

    colorAtPixel :: Num p => Line -> Shape -> p
    colorAtPixel l s 
        | intersect == [] = 0
        | otherwise = colour
        where 
            intersect = intersections l s
            colour = 100;

    generatePixel :: Line -> Shape -> PixelRGB8
    generatePixel line sphere = PixelRGB8 
        ( (colorAtPixel line sphere)) 
        ( (colorAtPixel line sphere)) 
        128

    parseObjects inputJSON = 
        case decode inputJSON of
            Just objects -> objects
            Nothing -> Sphere (Point 0 0 0) 1



