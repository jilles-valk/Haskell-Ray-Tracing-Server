module Shapes
(
    Point(..),
    Vector(..),
    Line(..),
    Shape(..),
    moveP,
    toUnitVector,
    moveS,
    addV,
    subtractV,
    getPointOnLine,
    dot,
    intersections
    -- rotate
) where 

    data Point = Point Float Float Float deriving (Eq, Show)
    data Vector = Vector Float Float Float deriving (Eq, Show)
    data Line = Line Point Vector deriving (Eq, Show)
    data Shape = Sphere Point Float deriving (Eq, Show)
    -- and possibly more shapes

    toUnitVector :: Vector -> Vector
    toUnitVector (Vector x y z) = Vector (x/magnitude) (y/magnitude) (z/magnitude)
        where magnitude = sqrt(x^2 + y^2 + z^2)

    moveP :: Point -> Vector -> Point
    moveP (Point xa ya za) (Vector xb yb zb) = Point (xa+xb) (ya+yb) (za+zb)

    addV :: Vector -> Vector -> Vector
    addV (Vector xa ya za) (Vector xb yb zb) = Vector (xa+xb) (ya+yb) (za+zb)

    subtractV :: Vector -> Vector -> Vector
    subtractV (Vector xa ya za) (Vector xb yb zb) = Vector (xa-xb) (ya-yb) (za-zb)

    moveS :: Shape -> Vector -> Shape
    moveS (Sphere (Point xa ya za) r) (Vector xb yb zb) = 
        Sphere (Point (xa+xb) (ya+yb) (za+zb)) r

    getPointOnLine :: Line -> Float -> Point
    getPointOnLine (Line (Point xp yp zp) (Vector xv yv zv)) t = 
        Point (xp+xv*t) (yp+yv*t) (yp+yv*t)

    dot :: Vector -> Vector -> Float
    dot (Vector xa ya za) (Vector xb yb zb) = (xa*xb) + (ya*yb) + (za*zb)

    intersections :: Line -> Shape -> [Float]
    intersections (Line (Point xp yp zp) vp) (Sphere (Point xs ys zs) r)
        | discriminant < 0  = []
        | otherwise         = [t1, t2]
        where 
            sphereOriginRayOrigin = (Vector xp yp zp) `subtractV` (Vector xs ys zs)
            a = dot vp vp
            b = 2 * dot vp sphereOriginRayOrigin
            c = (dot sphereOriginRayOrigin sphereOriginRayOrigin) - r^2
            discriminant = b^2 - 4*a*c
            t1 = (-b - sqrt(discriminant))/(2*a)
            t2 = (-b + sqrt(discriminant))/(2*a)