module View 
(
    View (..), 
    generateLines
) where 
    import Data.Colour
    import Shapes
    
    data View = View Point Vector Vector Integer Integer Float Float

    generateLines :: View -> [Line]
    generateLines (View (Point xp yp zp) (Vector xv yv zv) (Vector xUp yUp zUp) hPixels vPixels fieldOfView aspectRatio)
        | fieldOfView == 0 = [Line centerPoint (Vector xv yv zv)]
        | otherwise = theLines
        where
            centerPoint = Point xp yp zp
            vNormal = Vector xv yv zv
            vUp = Vector xUp yUp zUp
            w = 2*tan(fieldOfView/2)
            h = w*aspectRatio
            distCenterCorner = sqrt((w/2)^2 + (h/2)^2)
            topAngle = atan(w/h)
            topLeft = getPointOnLine (Line centerPoint 
                (rotateV vUp vNormal (2*pi - topAngle))) distCenterCorner
            topRight = getPointOnLine (Line centerPoint 
                (rotateV vUp vNormal (topAngle))) distCenterCorner
            leftLine =  Line topLeft (Vector (-xUp) (-yUp) (-zUp))
            rightLine = Line topRight (Vector (-xUp) (-yUp) (-zUp))
            viewPoint = getPointOnLine (Line centerPoint (Vector (-xv) (-yv) (-zv))) 1.0

            theLines = [lineFromPoints viewPoint (getPointOnLine (lineFromPoints 
                (getPointOnLine leftLine y) (getPointOnLine rightLine y)) x) |
                y <- [0.0.. fromInteger (vPixels - 1)], x <- [0.0.. fromInteger (hPixels - 1)]]


    -- data ViewPoint = Point Float Float Float deriving (Eq, Show)
    -- data Pixel = Pixel Point deriving (Eq, Show)