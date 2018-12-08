module Main (
    main
) where

import           Criterion.Main
import           Criterion.Types
import           PORR.Mnist.Sequential

-- | ENTRY POINT
main :: IO ()
main = defaultMainWith defaultConfig { reportFile = Just "benchmark/sequential-bench.html" } [
    bench "MNIST perceptrons commitee accuracy" $ whnfIO $ testAccuracy 20
    ]
