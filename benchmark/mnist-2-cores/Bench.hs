module Main (
    main
) where

import           Criterion.Main
import           Criterion.Types
import           PORR.Mnist.MultiCore

-- | ENTRY POINT
main :: IO ()
main = defaultMainWith (defaultConfig { timeLimit = 100, reportFile = Just "benchmark/2-cores-benchmark.html" }) [
    bench "MNIST perceptrons commitee accuracy" $ whnfIO $ testAccuracy 20
    ]
