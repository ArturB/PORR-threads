module Main (
    main
) where

import           Criterion.Main
import           Criterion.Types
import           PORR.Mnist.MultiCore

-- | ENTRY POINT
main :: IO ()
main = defaultMainWith (defaultConfig { resamples = 1 }) [
    bench "MNIST perceptrons commitee accuracy" $ whnfIO $ testAccuracy 20
    ]
