module View 
(
    ViewPoint(..)
) where 
    import Data.Colour
    import Shapes
    
    data ViewPoint = Point Float Float Float deriving (Eq, Show)
    data Pixel = Pixel Point deriving (Eq, Show)