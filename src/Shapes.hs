{-# LANGUAGE DeriveGeneric #-}
module Shapes
(
    Point(..),
    Vector(..),
    Line(..),
    Shape(..),
    Lightsource(..),
    moveP,
    toUnitVector,
    moveS,
    addV,
    subtractV,
    getPointOnLine,
    dot,
    timesV,
    rotateV,
    intersections,
    lineFromPoints
    -- rotate
) where 
    import GHC.Generics
    import Data.Aeson
    import Data.Aeson.Types
    import Data.Colour.SRGB

    data Point = Point {
        x :: Float,
        y :: Float,
        z :: Float
    } deriving (Eq, Show, Generic)
    data Vector = Vector {
        xv :: Float,
        yv :: Float,
        zv :: Float
    } deriving (Eq, Show, Generic)
    data Line = Line {
        point :: Point,
        direction :: Vector
    } deriving (Eq, Show)
    data Shape = Sphere {
        center :: Point,
        radius :: Float
    } deriving (Eq, Show, Generic)
    data Lightsource = Lightsource {
        location :: Point,
        intensity :: Float
    } deriving (Eq, Show, Generic)
    -- and possibly more shapes
    instance ToJSON Point where
        toJSON = genericToJSON defaultOptions 
    instance FromJSON Point where
        parseJSON = genericParseJSON defaultOptions 
    instance ToJSON Vector where
        toJSON = genericToJSON defaultOptions 
    instance FromJSON Vector where
        parseJSON = genericParseJSON defaultOptions 
    instance ToJSON Shape where
        toJSON = genericToJSON defaultOptions
    instance FromJSON Shape where
        parseJSON = genericParseJSON defaultOptions 
    instance ToJSON Lightsource where
        toJSON = genericToJSON defaultOptions
    instance FromJSON Lightsource where
        parseJSON = genericParseJSON defaultOptions 

    toUnitVector :: Vector -> Vector
    toUnitVector (Vector x y z) = Vector (x/magnitude) (y/magnitude) (z/magnitude)
        where magnitude = sqrt(x^2 + y^2 + z^2)

    moveP :: Point -> Vector -> Point
    moveP (Point xa ya za) (Vector xb yb zb) = Point (xa+xb) (ya+yb) (za+zb)

    addV :: Vector -> Vector -> Vector
    addV (Vector xa ya za) (Vector xb yb zb) = Vector (xa+xb) (ya+yb) (za+zb)

    subtractV :: Vector -> Vector -> Vector
    subtractV (Vector xa ya za) (Vector xb yb zb) = Vector (xa-xb) (ya-yb) (za-zb)

    rotateV :: Vector -> Vector -> Float-> Vector
    rotateV v k theta=
        (v `timesV` cos(theta)) `addV` ((k `crossV` v) `timesV` sin(theta)) `addV`
            (k `timesV` ((k `dot` v) * (1 - cos(theta))))

    dot :: Vector -> Vector -> Float
    dot (Vector xa ya za) (Vector xb yb zb) = (xa*xb) + (ya*yb) + (za*zb)

    timesV :: Vector -> Float -> Vector
    timesV (Vector x y z) s = Vector (x*s) (y*s) (z*s)

    crossV :: Vector -> Vector -> Vector
    crossV (Vector xa ya za) (Vector xb yb zb) = 
        Vector (ya*zb - za*yb) (-(xa*zb - za*xb)) (xa*yb - ya*xb)

    moveS :: Shape -> Vector -> Shape
    moveS (Sphere (Point xa ya za) r) (Vector xb yb zb) = 
        Sphere (Point (xa+xb) (ya+yb) (za+zb)) r

    getPointOnLine :: Line -> Float -> Point
    getPointOnLine (Line (Point xp yp zp) (Vector xv yv zv)) t = 
        Point (xp+xv*t) (yp+yv*t) (zp+zv*t)

    lineFromPoints :: Point -> Point -> Line
    lineFromPoints (Point xa ya za) (Point xb yb zb) = 
        Line (Point xa ya za) (toUnitVector ((Vector xb yb zb) `subtractV` (Vector xa ya za)))

    intersections :: Line -> Shape -> [Float]
    intersections (Line (Point xp yp zp) vp) (Sphere (Point xs ys zs) r)
        | discriminant < 0  = []
        | t1 < t2           = [t1, t2]
        | otherwise         = [t2, t1]
        where 
            sphereOriginRayOrigin = (Vector xp yp zp) `subtractV` (Vector xs ys zs)
            a = dot vp vp
            b = 2 * dot vp sphereOriginRayOrigin
            c = (dot sphereOriginRayOrigin sphereOriginRayOrigin) - r^2
            discriminant = b^2 - 4*a*c
            t1 = (-b - sqrt(discriminant))/(2*a)
            t2 = (-b + sqrt(discriminant))/(2*a)

    getReflection :: Line -> Point -> Shape -> Line
    getReflection line reflectionPoint sphere = 
        let normal = lineFromPoints (center sphere) reflectionPoint

        in Line (Point 1 2 3) (Vector 1 2 3)