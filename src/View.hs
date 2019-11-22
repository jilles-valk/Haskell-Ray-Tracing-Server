{-# LANGUAGE DeriveGeneric #-}
module View 
(
    View (..), 
    generateLines,
    generateLines2,
    generateLines3,
    generateLinesHelper
) where 
    import GHC.Generics
    import Data.Aeson
    import Data.Aeson.Types
    import Data.Colour
    import Shapes
    
    data View = View {
        position ::Point,
        forwardVector :: Vector,
        upVector :: Vector,
        horPixels :: Integer,
        verPixels :: Integer,
        fieldOfView :: Float
     } deriving (Eq, Show, Generic)
    instance ToJSON View where
        toJSON = genericToJSON defaultOptions 
    instance FromJSON View where
        parseJSON = genericParseJSON defaultOptions 

    generateLines :: View -> [Line]
    generateLines (View (Point xp yp zp) (Vector xv yv zv) (Vector xUp yUp zUp) horPixels verPixels fieldOfView)
        | fieldOfView == 0 = [Line centerPoint (Vector xv yv zv)]
        | otherwise = theLines
        where
            centerPoint = Point xp yp zp
            vNormal = Vector xv yv zv
            vUp = Vector xUp yUp zUp
            height = 2*tan(fieldOfView/2)
            width = height/((fromInteger horPixels)/(fromInteger verPixels))
            distCenterCorner = sqrt((height/2)^2 + (width/2)^2)
            topAngle = atan(height/width)
            topLeft = getPointOnLine (Line centerPoint 
                (rotateV vUp vNormal (2*pi - topAngle))) distCenterCorner
            topRight = getPointOnLine (Line centerPoint 
                (rotateV vUp vNormal (topAngle))) distCenterCorner
            leftLine =  Line topLeft (Vector (-xUp) (-yUp) (-zUp))
            rightLine = Line topRight (Vector (-xUp) (-yUp) (-zUp))
            viewPoint = getPointOnLine (Line centerPoint (Vector (-xv) (-yv) (-zv))) 1.0

            theLines = [lineFromPoints viewPoint (getPointOnLine (lineFromPoints 
                (getPointOnLine leftLine ((x*width)/(fromInteger (verPixels - 1)))) 
                (getPointOnLine rightLine ((x*width)/(fromInteger (verPixels - 1))))) 
                ((y*height)/(fromInteger (horPixels - 1))) ) |
                x <- [0.0.. fromInteger (verPixels - 1)], y <- [0.0.. fromInteger (horPixels - 1)]]
    
    generateLines3 :: View -> Float -> Float -> [Line]
    generateLines3 (View (Point xp yp zp) (Vector xv yv zv) (Vector xUp yUp zUp) horPixels vPixels fieldOfView) part numParts
        | fieldOfView == 0 = [Line centerPoint (Vector xv yv zv)]
        | otherwise = theLines
        where
            verPixels = toInteger $ round ((fromInteger vPixels)/numParts)
            -- verPixels = 2
            centerPoint = Point xp yp zp
            vNormal = Vector xv yv zv
            vUp = Vector xUp yUp zUp
            height = 2*tan(fieldOfView/2)
            width = height/((fromInteger horPixels)/(fromInteger verPixels))
            distCenterCorner = sqrt((height/2)^2 + (width/2)^2)
            topAngle = atan(height/width)
            topLeftWhole = getPointOnLine (Line centerPoint 
                (rotateV vUp vNormal (2*pi - topAngle))) distCenterCorner
            topRightWhole = getPointOnLine (Line centerPoint 
                (rotateV vUp vNormal (topAngle))) distCenterCorner
            down = (Vector (-xUp) (-yUp) (-zUp))
            leftLineWhole =  Line topLeftWhole (Vector (-xUp) (-yUp) (-zUp))
            rightLineWhole = Line topRightWhole (Vector (-xUp) (-yUp) (-zUp))
            leftLine = Line (getPointOnLine leftLineWhole ((part-1)*numParts*height)) down
            rightLine = Line (getPointOnLine rightLineWhole ((part-1)*numParts*height)) down
            viewPoint = getPointOnLine (Line centerPoint (Vector (-xv) (-yv) (-zv))) 1.0

            theLines = [lineFromPoints viewPoint (getPointOnLine (lineFromPoints 
                (getPointOnLine leftLine ((x*width)/(fromInteger (verPixels - 1)))) 
                (getPointOnLine rightLine ((x*width)/(fromInteger (verPixels - 1))))) 
                ((y*height)/(fromInteger (horPixels - 1))) ) |
                x <- [0.0.. fromInteger (verPixels - 1)], y <- [0.0.. fromInteger (horPixels - 1)]]

    generateLines2 :: View -> [Line]
    generateLines2 (View centerPoint directionForward directionUp horPixels verPixels fieldOfView) = 
        let viewPoint = centerPoint `moveP` (directionForward `timesV` (-1) )
            viewPortWidth = 2*tan(fieldOfView/2)
            viewPortHeight = viewPortWidth*((fromInteger verPixels)/(fromInteger horPixels))
            directionLeftStep = (rotateV directionUp directionForward (-0.5*pi)) `timesV` (viewPortWidth/(fromInteger horPixels))
            directionUpStep = directionUp `timesV` (viewPortHeight/(fromInteger verPixels))
            startPoint = centerPoint 
                `moveP` 
                    ((directionLeftStep `timesV` (-0.5* (fromInteger horPixels)))
                    `addV` 
                    (directionUpStep `timesV` (-0.5*(fromInteger verPixels))))
            firstLine = lineFromPoints viewPoint startPoint
        in generateLinesHelper viewPoint directionLeftStep directionUpStep startPoint startPoint horPixels verPixels 1 [firstLine]

    
    generateLinesHelper :: Point -> Vector -> Vector -> Point -> Point -> Integer -> Integer -> Integer -> [Line] -> [Line]
    generateLinesHelper viewPoint directionLeft directionUp lastPoint lastStartPoint horPixels verPixels 
        numPixelsDone lines
        | numPixelsDone == (horPixels * verPixels) = lines
        | (numPixelsDone `mod` horPixels) == 0 = 
            generateLinesHelper viewPoint directionLeft directionUp newLastStartPoint newLastStartPoint 
            horPixels verPixels (numPixelsDone + 1) (nextRowLine:lines)
        | otherwise = 
            generateLinesHelper viewPoint directionLeft directionUp newLastPoint lastStartPoint 
            horPixels verPixels (numPixelsDone + 1) (newLine:lines)
        where
            newLastPoint = lastPoint `moveP` directionLeft
            newLine = lineFromPoints viewPoint newLastPoint
            newLastStartPoint = lastStartPoint `moveP` directionUp
            nextRowLine = lineFromPoints viewPoint newLastStartPoint
