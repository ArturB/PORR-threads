module Main (
    main
) where

import           Criterion.Main
import           PORR.Mnist.MultiCore

-- Number of iterations the perceptron is learned
learnIters :: Int
learnIters = 20

-- | ENTRY POINT
main :: IO ()
main = defaultMain [
    bench "MNIST perceptrons commitee accuracy" $ whnfIO $ testAccuracy learnIters
    ]
