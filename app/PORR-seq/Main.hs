module Main (
    main
) where

import           Control.DeepSeq
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Class
import           Data.Maybe
import           Multilinear
import           Multilinear.Generic
import           Multilinear.Generic.Serialize
import           Multilinear.Index
import qualified Multilinear.Matrix            as Matrix
import           Perceptron
--import           Statistics.Distribution.Normal
import           System.IO

-- Number of iterations the perceptron is learned
learnIters :: Int
learnIters = 20

-- | Generate vector of binary responses (1,0) for MNIST labels vector
mnistBinaryResponses :: 
    Tensor Double -- ^ MNIST labels as read from CSV
 -> [Int]         -- ^ Digits belonging to "positive" response class
 -> Tensor Double -- ^ MNIST network responses, coded in 1-N
mnistBinaryResponses ls ps = 
    let ps' = fromIntegral <$> ps
    in  (\x -> if x `elem` ps' then 1.0 else 0.0) `Multilinear.map` ls

-- | Return number of images (indexed by lower index) in given data
imagesNum :: Tensor Double -> Int
imagesNum t = 
    let t' = (t $| ("i","t")) <<<| "t"
    in  fromJust $ indexSize $ head $ indices t'

-- | Learn binary perceptron with given training and testing data
-- | Training images and labels are assumed they contain examples only of positive and negative class
learnBinaryPerceptron :: 
    Tensor Double -- ^ training images
 -> Tensor Double -- ^ training labels
 -> (Int,Int)     -- ^ Positive and negative class digit
 -> Tensor Double 
learnBinaryPerceptron trainImages trainLabels (pos,_) =
    let trainResponses = mnistBinaryResponses trainLabels [pos]
        w0 = Matrix.const "ij" 1 40 0.0
    in  perceptron trainImages trainResponses w0 learnIters

-- | Learn binary perceptron and test its accuracy
-- | Learn from all MNIST images, filtering only these for positive and negative class
-- | Prints progress and result on stdout
testBinaryPerceptron :: 
    Tensor Double -- ^ all training images
 -> Tensor Double -- ^ all training labels
 -> Tensor Double -- ^ all test images
 -> Tensor Double -- ^ all test labels
 -> (Int,Int)     -- ^ Positive and negative class digit
 -> IO () 
testBinaryPerceptron trainImages trainLabels t10kImages t10kLabels cs@(pos_,neg_) = do
    let pos = pos_ + 1 -- MNIST digits are labelled such: 0 -> label 1, 9 -> label 10
    let neg = neg_ + 1
    
    -- from all MNIST images and labels, filter only required 
    let train25Images = Multilinear.filterIndex "t" 
            (\i -> (trainLabels $$| ("t",[i])) `elem` [Scalar $ fromIntegral pos, Scalar $ fromIntegral neg] ) trainImages
    let train25Labels = Multilinear.filterIndex "t" 
            (\i -> (trainLabels $$| ("t",[i])) `elem` [Scalar $ fromIntegral pos, Scalar $ fromIntegral neg] ) trainLabels
    let t10k25Images = Multilinear.filterIndex "t" 
            (\i -> (t10kLabels $$| ("t",[i])) `elem` [Scalar $ fromIntegral pos, Scalar $ fromIntegral neg] ) t10kImages
    let t10k25Labels = Multilinear.filterIndex "t" 
            (\i -> (t10kLabels $$| ("t",[i])) `elem` [Scalar $ fromIntegral pos, Scalar $ fromIntegral neg] ) t10kLabels
    
    
    -- learn perceptron and calculate output for testing data
    let p = learnBinaryPerceptron train25Images train25Labels (pos,neg)
    let y = sgn `Multilinear.map` (p $| ("i","j") * t10k25Images $| ("j","t"))
    let expectedResponses  = mnistBinaryResponses t10k25Labels [pos]
    let nT10k = t10k25Labels `Multilinear.size` "t"
    let accuracy = sum ((\i -> 
            if y $| ("i","t") $$| ("it",[0,i]) == expectedResponses $| ("i","t") $$| ("t",[i]) 
            then 1.0 
            else 0.0) 
            <$> [0 .. nT10k - 1]) / fromIntegral nT10k

    putStrLn $ "\nBinary perceptron for " ++ show cs ++ " learned!"
    putStrLn $ "Accuracy = " ++ show (floor (1000 * accuracy) `div` 10) ++ "%"


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
    let trainLabels' = (trainLabels $| ("i","t")) $$| ("i",[0]) 
    let t10kLabels'  = (t10kLabels $| ("i","t")) $$| ("i",[0]) 
    -- learn perceptron and test accuracy
    lift $ putStrLn ""
    lift $ putStrLn $ "Loaded CSV files. Learning perceptron...\nIterations: " ++ show learnIters
    lift $ putStrLn $ "Training images: " ++ show (imagesNum trainImages)
    lift $ hFlush stdout
    let numPairs = uncurry (<) `Prelude.filter` (pure (,) <*> [0..9] <*> [0..9])
    let learnAllPairs = mapM_ (testBinaryPerceptron trainImages trainLabels' t10kImages t10kLabels') numPairs
    lift learnAllPairs


-- | ENTRY POINT
main :: IO ()
main = do
    runExceptT getCSV
    return ()
    
