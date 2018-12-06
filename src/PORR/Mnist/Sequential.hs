module PORR.Mnist.Sequential (
    getMnistCSV
) where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Class
import           Multilinear.Class
import           Multilinear.Generic.Sequential
import           Multilinear.Generic.Sequential.Serialize
import           System.IO

-- | read MNIST images from files
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
