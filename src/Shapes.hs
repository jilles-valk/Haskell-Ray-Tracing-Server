module Shapes
(
    Point(..),
    Vector(..),
    Line(..),
    Shape(..),
    moveP,
    moveS,
    addV,
    subtractV
    -- rotate
) where 

    data Point = Point Float Float Float deriving (Eq, Show)
    data Vector = Vector Float Float Float deriving (Eq, Show)
    data Line = Line Point Vector 
    data Shape = Sphere Point Float deriving (Eq, Show)
    -- and possibly more shapes

    moveP :: Point -> Vector -> Point
    moveP (Point xa ya za) (Vector xb yb zb) = Point (xa+xb) (ya+yb) (za+zb)

    addV :: Vector -> Vector -> Vector
    addV (Vector xa ya za) (Vector xb yb zb) = Vector (xa+xb) (ya+yb) (za+zb)

    subtractV :: Vector -> Vector -> Vector
    subtractV (Vector xa ya za) (Vector xb yb zb) = Vector (xa-xb) (ya-yb) (za-zb)

    moveS :: Shape -> Vector -> Shape
    moveS (Sphere (Point xa ya za) r) (Vector xb yb zb) = 
        Sphere (Point (xa+xb) (ya+yb) (za+zb)) r