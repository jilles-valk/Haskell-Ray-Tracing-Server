{-# LANGUAGE DeriveGeneric #-}
module Render 
(
    render,
    renderTest,
    makeViewTest,
    colorAtPixel,
    generatePixel,
    getNearestIntersectingObject,
    parseScene,
    addIntensity
)
where 
    import qualified Data.ByteString.Lazy as BL
    import GHC.Generics
    import Data.Aeson
    import Data.Aeson.Types
    import Shapes
    import Data.Colour.SRGB
    import View
    import Codec.Picture
    import Data.Aeson
    import Data.Aeson.Types
    import Data.Maybe

    data Scene = Scene {
        view :: View,
        objects :: [Shape],
        lightsources :: [Lightsource]
     } deriving (Eq, Show, Generic)
    instance ToJSON Scene where
        toJSON = genericToJSON defaultOptions 
    instance FromJSON Scene where
        parseJSON = genericParseJSON defaultOptions 

    render :: BL.ByteString -> BL.ByteString
    render inputJSON     
        = encodePng $ snd $ generateFoldImage 
            (\deAcc x y -> generatePixel deAcc objectList lightsourceList) lines
            (fromInteger hPixels) (fromInteger vPixels)
        where 
            scene = parseScene inputJSON
            objectList = objects scene
            lightsourceList = lightsources scene
            camera = view scene
            hPixels = horPixels camera
            vPixels = verPixels camera
            lines = generateLines camera

    colorAtPixel ::Line -> (Maybe Shape, [Float]) -> [Shape] -> [Lightsource] -> Float
    colorAtPixel l object objectList lightsources
        | isJust $ fst object = intensity
        | otherwise = 0
        where 
            pointOnObject = getPointOnLine l $ head $ snd object
            intensityLightsourceAndLine = addIntensity 0 pointOnObject objectList lightsources
            reflection = getReflection l pointOnObject (fromJust (fst object))
            
            angleReflectionLightsource = acos( (direction reflection) `dot` (direction $ snd intensityLightsourceAndLine))
            intensity = fst intensityLightsourceAndLine * 
                (((2 + angleReflectionLightsource)/(angleReflectionLightsource + 1)) -1)
            
    addIntensity :: Float -> Point -> [Shape] -> [Lightsource] -> (Float, Line)
    addIntensity accIntensity _ [] _ = (accIntensity, (Line (Point 1 2 3) (Vector 1 2 3)))
    addIntensity accIntensity _ _ [] = (accIntensity, (Line (Point 1 2 3) (Vector 1 2 3)))
    addIntensity accIntensity pointOnObject objectList [justOneLightsource]
        | blocked = (accIntensity, objectToLightsource)
        | otherwise = (newIntensity, objectToLightsource)
        where 
            objectToLightsource = lineFromPoints pointOnObject $ location justOneLightsource
            -- blocked = isJust $ fst $ getNearestIntersectingObject objectToLightsource objectList
            blocked = False
            newIntensity = accIntensity + intensity justOneLightsource
    addIntensity accIntensity pointOnObject objectList (nextLightsource:lightsources)
        | blocked = addIntensity accIntensity pointOnObject objectList lightsources
        | otherwise = addIntensity newIntensity pointOnObject objectList lightsources
        where 
            objectToLightsource = lineFromPoints pointOnObject $ location nextLightsource
            blocked = isJust $ fst $ getNearestIntersectingObject objectToLightsource objectList
            -- blocked = True
            invAccIntensity = accIntensity/(1-accIntensity)
            newIntensity = invAccIntensity + intensity nextLightsource

    getNearestIntersectingObject :: Line -> [Shape] -> (Maybe Shape, [Float])
    getNearestIntersectingObject _ [] = (Nothing, [])
    getNearestIntersectingObject line [oneObject] 
        | intersect == [] = (Nothing, [])
        | head intersect > (-0.01) && intersect !! 1 > (-0.01) = (Just oneObject, intersect)
        | otherwise = (Nothing, [])
        where 
            intersect = intersections line oneObject
    getNearestIntersectingObject line (firstObject:otherObjects) = 
        getNearestIntersectingObjectHelper line otherObjects 
        (firstObject, intersections line firstObject)

    getNearestIntersectingObjectHelper :: Line -> [Shape] -> (Shape, [Float]) -> (Maybe Shape, [Float])
    getNearestIntersectingObjectHelper line [oneObject] nearest = 
        getClosest line oneObject nearest
    getNearestIntersectingObjectHelper line (firstObject:otherObjects) nearest
        | isJust $ fst closest = getNearestIntersectingObjectHelper line otherObjects 
            (fromJust $ fst closest, snd closest)
        | otherwise = getNearestIntersectingObjectHelper line otherObjects nearest
        where 
            closest = getClosest line firstObject nearest

    getClosest :: Line -> Shape -> (Shape, [Float]) -> (Maybe Shape, [Float])
    getClosest line objectOne objectTwo
        | intersectionsOne == [] && intersectionsTwo == [] = (Nothing, [])
        | intersectionsOne == [] && isObjectTwoBeforeCamera = (Just $ fst objectTwo, intersectionsTwo)
        | intersectionsTwo == [] && isObjectOneBeforeCamera = (Just objectOne, intersectionsOne)
        | oneCloser && isObjectOneBeforeCamera = (Just objectOne, intersectionsOne)
        | isObjectTwoBeforeCamera = (Just $ fst objectTwo, intersectionsTwo)
        | otherwise = (Nothing, [])
        where
            intersectionsOne = intersections line objectOne
            intersectionsTwo = intersections line $ fst objectTwo
            isObjectOneBeforeCamera = (head intersectionsOne) > 0 || (intersectionsOne !! 1) > 0
            isObjectTwoBeforeCamera = (head intersectionsTwo) > 0 || (intersectionsTwo !! 1) > 0
            oneCloser = head intersectionsOne < head intersectionsTwo

    generatePixel :: [Line] -> [Shape] -> [Lightsource]-> ([Line], PixelRGB8)
    generatePixel (nextInLine:lines) objectList lightsources = (lines, PixelRGB8 
       intensity intensity intensity)
        where
            intensity = round ((colorAtPixel nextInLine nearestIntersectingObject objectList lightsources) * 255)
            nearestIntersectingObject = getNearestIntersectingObject nextInLine objectList

    parseScene :: BL.ByteString -> Scene
    parseScene inputJSON = 
        case decode inputJSON of
            Just scene -> scene
            Nothing ->  (Scene 
                            (View 
                                (Point 0 0 3) 
                                (Vector 0 0 (-1)) 
                                (Vector 0 1 0) 
                                300 
                                200 
                                (0.5*pi) 
                            ) 
                            [
                                (Sphere 
                                    (Point (-1) 0 0) 1), 
                                (Sphere 
                                    (Point 1 0 0) 1)
                            ]
                            [(Lightsource (Point 0 0 3) 0.5)])

    -- renderTest :: Int -> Image
    makeViewTest size = do 
            let view = View (Point 0 0 3) (Vector 0 0 (-1)) (Vector 0 1 0) size size (0.5*pi)
            let lines = generateLines view
            return $! lines

    renderTest size isRecursive
        | isRecursive =  writePng "img1.png" $ snd (generateFoldImage 
            (\deAcc x y -> generatePixel deAcc sphere lightsourceList) linesRec
            (fromInteger hPixels) (fromInteger vPixels))
        | otherwise =  writePng "img1.png" $ snd (generateFoldImage 
            (\deAcc x y -> generatePixel deAcc sphere lightsourceList) linesListComprehension
            (fromInteger hPixels) (fromInteger vPixels))
        where 
            sphere = [Sphere (Point 0 0 0) 1]
            lightsourceList = [Lightsource (Point 0 0 3) 0.5]
            hPixels = size
            vPixels = size
            view = View (Point 0 0 3) (Vector 0 0 (-1)) (Vector 0 1 0) hPixels vPixels (0.5*pi)
            linesRec = generateLines2 view
            linesListComprehension = generateLines view