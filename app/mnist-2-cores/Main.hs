module Main (
    main
) where

import           PORR.Mnist.MultiCore

-- | ENTRY POINT
main :: IO ()
main = testAccuracy 20
