module Render 
(
    render,
    renderTest,
    makeViewTest,
    colorAtPixel,
    generatePixel,
    getNearestIntersectingObject
)
where 
    import Shapes
    import Data.Colour 
    import View
    import Codec.Picture
    import Data.Aeson
    import Data.Aeson.Types
    import Data.Maybe

    render inputJSON 
        -- | inputString == _ = image
        -- | otherwise         
        = encodePng $ generateImage 
        (\x y -> generatePixel (lines !! x !! y) objects) 
        (fromInteger hPixels) (fromInteger vPixels)
        where 
            objects = parseObjects inputJSON
            hPixels = 300
            vPixels = 200
            view = View (Point 0 0 3) (Vector 0 0 (-1)) (Vector 0 1 0) hPixels vPixels (0.5*pi) (3/2)
            lines = generateLines view
            -- image = encodePng $ generateImage 
            --     (\x y -> generatePixel x y (lines !! x !! y) sphere) 
            --     (fromInteger vPixels) (fromInteger hPixels)

    colorAtPixel :: Num p => Line -> Maybe Shape -> p
    colorAtPixel l object 
        | isJust object = 100
        | otherwise = 0

    getNearestIntersectingObject :: Line -> [Shape] -> Maybe Shape
    getNearestIntersectingObject line [] = Nothing
    getNearestIntersectingObject line [oneObject] 
        |   intersect == [] = Nothing
        |   otherwise = Just oneObject
        where 
            intersect = intersections line oneObject
    getNearestIntersectingObject line (firstObject:otherObjects) = 
        getNearestIntersectingObjectHelper line otherObjects firstObject

    getNearestIntersectingObjectHelper :: Line -> [Shape] -> Shape -> Maybe Shape
    getNearestIntersectingObjectHelper line [oneObject] nearest = 
        getClosest line oneObject nearest
    getNearestIntersectingObjectHelper line (firstObject:otherObjects) nearest
        | isJust closest = getNearestIntersectingObjectHelper line otherObjects $ fromJust closest
        | otherwise = getNearestIntersectingObjectHelper line otherObjects nearest
        where 
            closest = getClosest line firstObject nearest

    getClosest :: Line -> Shape -> Shape -> Maybe Shape
    getClosest line objectOne objectTwo
        | intersectionsOne == [] && intersectionsTwo == [] = Nothing
        | intersectionsOne == [] = Just objectTwo
        | intersectionsTwo == [] = Just objectOne
        | oneCloser = Just objectOne
        | otherwise = Just objectTwo
        where
            intersectionsOne = intersections line objectOne
            intersectionsTwo = intersections line objectTwo
            oneCloser = head intersectionsOne < head intersectionsTwo


    generatePixel :: Line -> [Shape] -> PixelRGB8
    generatePixel line objects = PixelRGB8 
        ( (colorAtPixel line nearestIntersectingObject)) 
        ( (colorAtPixel line nearestIntersectingObject)) 
        128
        where
            nearestIntersectingObject = getNearestIntersectingObject line objects


    parseObjects inputJSON = 
        case decode inputJSON of
            Just objects -> objects
            Nothing -> [Sphere (Point 0 0 0) 1]

    -- renderTest :: Int -> Image
    makeViewTest size = do 
            let view = View (Point 0 0 3) (Vector 0 0 (-1)) (Vector 0 1 0) size size (0.5*pi) (3/2)
            let lines = generateLines view
            return $! lines

    renderTest size
        -- | inputString == _ = image
        -- | otherwise         
        =  writePng "img1.png" $ generateImage 
            (\x y -> generatePixel (lines !! x !! y) sphere) 
            (fromInteger hPixels) (fromInteger vPixels)
        where 
            sphere = [Sphere (Point 0 0 0) 1]
            hPixels = size
            vPixels = size
            view = View (Point 0 0 3) (Vector 0 0 (-1)) (Vector 0 1 0) hPixels vPixels (0.5*pi) (3/2)
            lines = generateLines view



