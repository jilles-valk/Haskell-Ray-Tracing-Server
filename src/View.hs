module View 
(
    View (..), 
    generateLines
) where 
    import Data.Colour
    import Shapes
    
    data View = View Point Vector Vector Integer Integer Float Float

    generateLines :: View -> [[Line]]
    generateLines (View (Point xp yp zp) (Vector xv yv zv) (Vector xUp yUp zUp) horPixels verPixels fieldOfView aspectRatio)
        | fieldOfView == 0 = [[Line centerPoint (Vector xv yv zv)]]
        | otherwise = theLines
        where
            centerPoint = Point xp yp zp
            vNormal = Vector xv yv zv
            vUp = Vector xUp yUp zUp
            height = 2*tan(fieldOfView/2)
            width = height/aspectRatio
            distCenterCorner = sqrt((height/2)^2 + (width/2)^2)
            topAngle = atan(height/width)
            topLeft = getPointOnLine (Line centerPoint 
                (rotateV vUp vNormal (2*pi - topAngle))) distCenterCorner
            topRight = getPointOnLine (Line centerPoint 
                (rotateV vUp vNormal (topAngle))) distCenterCorner
            leftLine =  Line topLeft (Vector (-xUp) (-yUp) (-zUp))
            rightLine = Line topRight (Vector (-xUp) (-yUp) (-zUp))
            viewPoint = getPointOnLine (Line centerPoint (Vector (-xv) (-yv) (-zv))) 1.0

            theLines = [[lineFromPoints viewPoint (getPointOnLine (lineFromPoints 
                (getPointOnLine leftLine ((x*width)/(fromInteger (verPixels - 1)))) 
                (getPointOnLine rightLine ((x*width)/(fromInteger (verPixels - 1))))) 
                ((y*height)/(fromInteger (horPixels - 1))) ) |
                x <- [0.0.. fromInteger (verPixels - 1)]] | y <- [0.0.. fromInteger (horPixels - 1)]]