module PORR2.Mnist.Sequential (
    getMnistCSV, testAccuracy
) where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Class
import           Multilinear.Class
import           Multilinear.Generic.Sequential
import           Multilinear.Generic.Sequential.Serialize
import           PORR2.Perceptron.Sequential
import           System.IO

-- | read MNIST images from files
{-# INLINE getMnistCSV #-}
getMnistCSV :: ExceptT String IO (Tensor Double, Tensor Double, Tensor Double, Tensor Double)
getMnistCSV = do
    -- read all input files
    lift $ putStrLn "Loading mnist/train-images.csv..." >> hFlush stdout
    trainImages :: Tensor Double <- fromCSVFile "mnist/train-images.csv" ',' "it"
    lift $ putStrLn "Loading mnist/train-labels.csv..." >> hFlush stdout
    trainLabels' :: Tensor Double <- fromCSVFile "mnist/train-labels.csv" ',' "it"
    lift $ putStrLn "Loading mnist/t10k-images.csv..." >> hFlush stdout
    t10kImages  :: Tensor Double <- fromCSVFile "mnist/t10k-images.csv"  ',' "it"
    lift $ putStrLn "Loading mnist/t10k-labels.csv..." >> hFlush stdout
    t10kLabels'  :: Tensor Double <- fromCSVFile "mnist/t10k-labels.csv"  ',' "it"
    -- MNIST labels should be 1D functional, but are read from CSV file always as 2D matrix
    let trainLabels = (trainLabels' $| ("","t")) $$| ("i",[0]) 
    let t10kLabels  = (t10kLabels'  $| ("","t")) $$| ("i",[0]) 
    lift $ return (trainImages, trainLabels, t10kImages, t10kLabels)

{-# INLINE testAccuracy #-}
testAccuracy :: Int -> IO ()
testAccuracy learnIters = do
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
    let accuracy = commiteeAccuracy commitee testImages testLabels
    putStrLn "\nBinary perceptrons commitee learned!"
    putStrLn $ "Accuracy = " ++ show accuracy ++ "%"
