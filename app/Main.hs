module Main where

import Multilinear
import Multilinear.Vector
import Multilinear.Matrix

main :: IO ()
main = do
    let v = Multilinear.Vector.fromIndices "i" 10 id
    print v
    
