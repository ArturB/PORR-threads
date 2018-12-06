module Main (
    main
) where

import           PORR.Mnist.Sequential

-- | ENTRY POINT
main :: IO ()
main = testAccuracy 20
