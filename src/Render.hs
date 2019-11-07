module Render 
(
    render
)
where 
    import Shapes
    import Data.Colour 
    import View

    render inputString 
        | inputString == [] = []
        | otherwise         = image 
        where 
            sphere = Sphere (Point 0 0 0) 1
            -- View = View (Point 0 0 5) (Vector 0 0 (-1)) 100 100 0.5*pi
            image = [1,2,3]

