module Main (
    main
) where

import           PORR.Mnist.GPU

-- | ENTRY POINT
main :: IO ()
main = testAccuracy 20
