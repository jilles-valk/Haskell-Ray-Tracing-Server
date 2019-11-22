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
    addIntensity,
    checkBlocked
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
    import Data.List.Split
    import Control.Parallel

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
        -- = encodePng $ snd $ generateFoldImage 
        --     (\deAcc x y -> takeFirst deAcc) (firstHalf `par` secondHalf `pseq` (firstHalf ++ secondHalf))
        --     (hPixels) (vPixels)
        where 
            scene = parseScene inputJSON
            objectList = objects scene
            lightsourceList = lightsources scene
            camera = view scene
            hPixels = fromInteger $ horPixels camera
            vPixels = fromInteger $ verPixels camera
            lines = generateLines camera
            -- linesInChunks = chunksOf ( ((hPixels*vPixels) `div` 2)) lines
            -- firstHalf = (createPixels [] (head linesInChunks) objectList lightsourceList)
            -- secondHalf = (createPixels [] (linesInChunks !! 1) objectList lightsourceList)
            -- allPixels =  firstHalf `par` secondHalf `pseq` (firstHalf ++ secondHalf)
            -- allPixels = firstHalf ++ secondHalf

    takeFirst :: [PixelRGB8] -> ([PixelRGB8], PixelRGB8)
    takeFirst [] = ([], PixelRGB8 0 0 0)
    takeFirst (firstPixel:otherPixels) = (otherPixels, firstPixel)
    
    colorAtPixel ::Line -> (Maybe Shape, [Float]) -> [Shape] -> [Lightsource] -> Float
    colorAtPixel l object objectList lightsources
        -- | isObject && angleReflectionLightsource > 0.5*pi = 0
        | isObject = intensityLightsource
        | otherwise = 0
        where 
            isObject = isJust $ fst object
            pointOnObject = getPointOnLine l $ head $ snd object
            intensityLightsource = addIntensity 0 l pointOnObject (fromJust (fst object)) objectList lightsources
            
    addIntensity :: Float -> Line -> Point -> Shape -> [Shape] -> [Lightsource] -> Float
    addIntensity _ _ _ _ _ [] = 0
    addIntensity accIntensity l pointOnObject object [] lightsourceList = 
        addIntensity accIntensity l pointOnObject object [object] lightsourceList
    addIntensity accIntensity l pointOnObject object objectList (nextLightsource:lightsources)
        | null lightsources && blocked = accIntensity
        | null lightsources && not blocked && lightedSide = newIntensity
        | not blocked && lightedSide = addIntensity newIntensity l pointOnObject object objectList lightsources
        | otherwise = addIntensity accIntensity l pointOnObject object objectList lightsources
        where 
            objectToLightsource = lineFromPoints pointOnObject $ location nextLightsource
            blocked = checkBlocked objectToLightsource objectList
            reflection = getReflection l pointOnObject object
            angleReflectionLightsource = acos((direction reflection) `dot` (direction objectToLightsource))
            angleLightsourceCamera = pi - acos((direction objectToLightsource) `dot` (timesV (direction l) (-1)))
            lightedSide = angleReflectionLightsource < (angleLightsourceCamera+0.01)
            adjustedAngle = (angleLightsourceCamera - angleReflectionLightsource)/angleLightsourceCamera
            invPrevIntensity = -((2*accIntensity)/(accIntensity - 1))
            newIntensity = (adjustedAngle* (intensity nextLightsource) + invPrevIntensity)/
                            (adjustedAngle* (intensity nextLightsource)  + invPrevIntensity+2)

    checkBlocked :: Line -> [Shape] -> Bool
    checkBlocked objectToLightsource [] = False
    checkBlocked objectToLightsource (firstObject:otherObjects)
        | doesIntersect && ((intersect > [-0.1] && intersect < [0.1]) || intersect > [0.1]) = True
        | otherwise = checkBlocked objectToLightsource otherObjects
        where
            intersect = intersections objectToLightsource firstObject
            doesIntersect = (null intersect) == False
        
    getNearestIntersectingObject :: Line -> [Shape] -> (Maybe Shape, [Float])
    getNearestIntersectingObject _ [] = (Nothing, [])
    getNearestIntersectingObject line [oneObject] 
        | intersect == [] = (Nothing, [])
        | intersect > [0,0] = (Just oneObject, intersect)
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
        | intersectionsOne == [] && intersectionsTwo /= [] &&
            isObjectTwoBeforeCamera = (Just $ fst objectTwo, intersectionsTwo)
        | intersectionsTwo == [] && intersectionsOne /= [] &&
            isObjectOneBeforeCamera = (Just objectOne, intersectionsOne)
        | someEmpty = (Nothing, [])
        | oneCloser && isObjectOneBeforeCamera = (Just objectOne, intersectionsOne)
        | isObjectTwoBeforeCamera = (Just $ fst objectTwo, intersectionsTwo)
        | otherwise = (Nothing, [])
        where
            intersectionsOne = intersections line objectOne
            intersectionsTwo = intersections line $ fst objectTwo
            someEmpty = (null intersectionsOne && null intersectionsTwo)
            isObjectOneBeforeCamera = ((null intersectionsOne) == False) && 
                (intersectionsOne > [0,0])
            isObjectTwoBeforeCamera = ((null intersectionsTwo) == False) && 
                (intersectionsTwo > [0,0])
            oneCloser = ( intersectionsOne) < ( intersectionsTwo)

    generatePixel :: [Line] -> [Shape] -> [Lightsource]-> ([Line], PixelRGB8)
    generatePixel (nextInLine:lines) objectList lightsources = 
        (lines, PixelRGB8 intensity intensity intensity)
        where
            intensity = round ((colorAtPixel nextInLine nearestIntersectingObject objectList lightsources) * 255)
            nearestIntersectingObject = getNearestIntersectingObject nextInLine objectList

    createPixels :: [PixelRGB8] -> [Line] -> [Shape] -> [Lightsource] -> [PixelRGB8]
    createPixels accPixels [] objectList lightsourceList = []
    createPixels accPixels (nextInLine:lines) objectList lightsourceList 
        | null lines = newPixel : accPixels
        | otherwise = createPixels (newPixel:accPixels) lines objectList lightsourceList
        where
            intensity = round ((colorAtPixel nextInLine nearestIntersectingObject objectList lightsourceList) * 255)
            nearestIntersectingObject = getNearestIntersectingObject nextInLine objectList
            newPixel = PixelRGB8 intensity intensity intensity

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

    force :: [a] -> ()
    force xs = go xs `pseq` ()
        where 
            go (_:xs) = go xs
            go [] = 1

    renderTest size isRecursive isParallel
        | not isRecursive && isParallel = writePng "img1.png" $ snd $ generateFoldImage 
            (\deAcc x y -> takeFirst deAcc) 
            allPixels
            (hPixels) (vPixels)
        | isRecursive =  writePng "img1.png" $ snd (generateFoldImage 
            (\deAcc x y -> generatePixel deAcc sphere lightsourceList) linesRec
            ( hPixels) ( vPixels))
        | otherwise =  writePng "img1.png" $ snd (generateFoldImage 
            (\deAcc x y -> generatePixel deAcc sphere lightsourceList) linesListComprehension
            ( hPixels) ( vPixels))
        where 
            sphere = [Sphere (Point 0 0 0) 1]
            lightsourceList = [Lightsource (Point 0 0 3) 0.5]
            hPixels = fromInteger size
            vPixels = fromInteger size
            view = View (Point 0 0 3) (Vector 0 0 (-1)) (Vector 0 1 0) size size (0.5*pi)
            view2 = View (Point 0 0 3) (Vector 0 0 (-1)) (Vector 0 1 0) 
                (size `div` 2) (size `div` 2) (0.5*pi)
            linesRec = generateLines2 view
            linesListComprehension = generateLines view
            linesInChunks = chunksOf ( ((hPixels*vPixels) `div` 2)) linesListComprehension
            lines1 = generateLines view2
            lines2 = generateLines view2
            lines3a = generateLines3 view 1 2
            lines3b = generateLines3 view 2 2
            firstHalf = (createPixels [] (lines3a) sphere lightsourceList)
            secondHalf = (createPixels [] (lines3b) sphere lightsourceList)
            allPixels = lines3a `seq` (lines3b `seq` (( firstHalf `par` ( secondHalf `pseq` (firstHalf ++ secondHalf)))))