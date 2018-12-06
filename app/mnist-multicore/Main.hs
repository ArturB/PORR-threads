module Main (
    main
) where

import           PORR.Mnist.MultiCore

-- Number of iterations the perceptron is learned
learnIters :: Int
learnIters = 20

-- | ENTRY POINT
main :: IO ()
main = testAccuracy learnIters
