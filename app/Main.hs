module Main where

import Lib
import Shapes

main :: IO ()
main = do 
    someFunc
    print (moveP (Point 1 2 3) (Vector 3 2 1))
