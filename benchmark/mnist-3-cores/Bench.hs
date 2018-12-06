module Main (
    main
) where

import           Criterion.Main
import           PORR.Mnist.MultiCore

-- | ENTRY POINT
main :: IO ()
main = defaultMain [
    bench "MNIST perceptrons commitee accuracy" $ whnfIO $ testAccuracy 20
    ]
