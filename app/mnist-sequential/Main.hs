module Main (
    main
) where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Class
import           Multilinear
import           Multilinear.Generic.Serialize
import           Perceptron
import           System.IO

-- Number of iterations the perceptron is learned
learnIters :: Int
learnIters = 20

-- | read MNIST images from files, then learn and test perceptron accuracy
getCSV :: ExceptT String IO ()
getCSV = do
    -- read all input files
    lift $ putStrLn "Loading mnist/train-images.csv..." >> hFlush stdout
    trainImages :: Tensor Double <- fromCSVFile "mnist/train-images.csv" ',' "it"
    lift $ putStrLn "Loading mnist/train-labels.csv..." >> hFlush stdout
    trainLabels :: Tensor Double <- fromCSVFile "mnist/train-labels.csv" ',' "it"
    lift $ putStrLn "Loading mnist/t10k-images.csv..." >> hFlush stdout
    t10kImages  :: Tensor Double <- fromCSVFile "mnist/t10k-images.csv"  ',' "it"
    lift $ putStrLn "Loading mnist/t10k-labels.csv..." >> hFlush stdout
    t10kLabels  :: Tensor Double <- fromCSVFile "mnist/t10k-labels.csv"  ',' "it"
    -- MNIST labels should be 1D functional, but are read from CSV file always as 2D matrix
    let trainLabels' = (trainLabels $| ("","t")) $$| ("i",[0]) 
    let t10kLabels'  = (t10kLabels $| ("","t")) $$| ("i",[0]) 
    -- learn perceptron commitee and test total commitee accuracy
    lift $ putStrLn ""
    lift $ putStrLn $ "Loaded CSV files. Learning perceptron...\nIterations: " ++ show learnIters ++ "\nPCA components:" ++ show (pcaNum trainImages)
    lift $ putStrLn $ "Training images: " ++ show (imagesNum trainImages)
    lift $ hFlush stdout
    -- generate 45 number pairs, according to 45 binary perceptrons commitee
    let numPairs = uncurry (<) `Prelude.filter` (pure (,) <*> [0..9] <*> [0..9])
    let commitee = (\(p,n) -> (p, n, learnBinaryPerceptron trainImages trainLabels' (p,n) learnIters)) <$> numPairs
    let commAnswers = commiteeAnswers commitee t10kImages
    -- check with referential labels
    let validAnswers = sum $ 
            (\i -> if t10kLabels' $$| ("t",[i]) == commAnswers $$| ("t",[i]) then 1 else 0) <$> [0..imagesNum t10kImages - 1]
    -- calculate and print perceptron accuracy
    let accuracy = validAnswers / fromIntegral (imagesNum t10kImages)
    let accuracy' = fromIntegral (floor (1000 * accuracy)) / 10
    lift $ putStrLn "\nBinary perceptrons commitee learned!"
    lift $ putStrLn $ "Accuracy = " ++ show accuracy' ++ "%"

-- | ENTRY POINT
main :: IO ()
main = do
    runExceptT getCSV
    return ()
    
