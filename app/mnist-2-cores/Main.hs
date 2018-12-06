module Main (
    main
) where

import           PORR2.Mnist.MultiCore

-- | ENTRY POINT
main :: IO ()
main = testAccuracy 20
