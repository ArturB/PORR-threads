module Main (
    main
) where

import qualified Control.Parallel.Strategies             as Parallel
import           Control.Monad.Trans.Except
import           PORR.Mnist.MultiCore
import           PORR.Perceptron.MultiCore
import           System.IO

-- Number of iterations the perceptron is learned
learnIters :: Int
learnIters = 20

-- | ENTRY POINT
main :: IO ()
main = do
    Right (trainImages, trainLabels, testImages, testLabels) <- runExceptT getMnistCSV
    -- learn perceptron commitee and test total commitee accuracy
    putStrLn $ 
         "\nLoaded CSV files. Learning perceptron...\n \
        \ Iterations: " ++ show learnIters ++ "\n \
        \ PCA components: " ++ show (pcaNum trainImages) ++ "\n \
        \ Training images: " ++ show (imagesNum trainImages)
    hFlush stdout
    -- generate 45 number pairs, according to 45 binary perceptrons commitee
    let numPairs = Prelude.filter (uncurry (<)) (pure (,) <*> [0..9] <*> [0..9])
    let commitee = fmap 
                   (\(p,n) -> (p, n, learnBinaryPerceptron trainImages trainLabels (p,n) learnIters)) 
                   numPairs
    let commitee' = commitee `Parallel.using` Parallel.parListChunk (45 `div` 8) Parallel.rdeepseq
    let accuracy = commiteeAccuracy commitee' testImages testLabels
    putStrLn "\nBinary perceptrons commitee learned!"
    putStrLn $ "Accuracy = " ++ show accuracy ++ "%"
