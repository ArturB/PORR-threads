module Main (
    main
) where

import           Control.DeepSeq
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Class
--import           Data.Foldable
import           Data.Maybe
import           Multilinear.Class
import           Multilinear.Generic
import           Multilinear.Generic.Serialize
import           Multilinear.Index
import qualified Multilinear.Matrix            as Matrix
import qualified Multilinear.Tensor            as Tensor
import qualified Multilinear.Vector            as Vector
import           Statistics.Distribution.Normal
import           System.IO

-- Number of iterations the perceptron is learned
ln :: Int
ln = 100

sgn :: (Num a, Ord a) => a -> a
sgn x = if x > 0 then 1 else 0

mnistResponses :: Tensor Double -- ^ MNIST labels as read from CSV
               -> Tensor Double -- ^ MNIST network responses, coded in 1-N
mnistResponses l = 
        -- MNIST responses are given as 1D functional, but read from CSV file always as 2D matrix
    let l' = (l $| ("i","t")) $$| ("i",[0]) 
        -- size of labels functional
        s = imagesNum l
        -- tensor of network 1-N responses
        rs = Tensor.generate ("",[]) ("t",[s]) $ 
            \_ [t] -> Vector.fromIndices "i" 10 $ 
                \i -> if fromIntegral i == l' $$| ("t",[t]) then 1.0 else 0.0
    in  rs |>>> "t" -- covariant index pushed deeper for efficiency reasons

imagesNum :: Tensor Double
          -> Int
imagesNum t = 
    let t' = (t $| ("i","t")) <<<| "t"
    in  fromJust $ indexSize $ head $ indices t'

nextWeights :: Tensor Double -- ^ actual inputs
            -> Tensor Double -- ^ expected outputs
            -> Tensor Double -- ^ current weights
            -> Tensor Double -- ^ next weights
nextWeights _x _e _w  = 
    let exNum = fromJust $ indexSize $ indices x !! 1
        x = _x $| ("i","t")
        e = _e $| ("a","t")
        w = _w $| ("a","i")
        y = sgn `Multilinear.Class.map` w * x -- y $| ("a","t")
        d = e - y -- d $| ("a","t")
        incW = x * d * Vector.const "t" exNum 1.0 -- incW $| ("ai","")
    in  incW `deepseq` w + incW \/ "i"

perceptron :: Tensor Double -- ^ Training images
           -> Tensor Double -- ^ Training labels
           -> Tensor Double -- ^ Initial weights
           -> Int           -- ^ Number of learning iterations
           -> Tensor Double -- ^ Trained weights
perceptron ts es w0 n = iterate (nextWeights ts es) w0 !! n

getCSV :: ExceptT String IO ()
getCSV = do
    lift $ putStrLn "Loading mnist/train-images.csv..." >> hFlush stdout
    trainImages :: Tensor Double <- fromCSVFile "mnist/train-images.csv" ',' "ij"
    lift $ putStrLn "Loading mnist/train-labels.csv..." >> hFlush stdout
    trainLabels :: Tensor Double <- fromCSVFile "mnist/train-labels.csv" ',' "ij"
    lift $ putStrLn "Loading mnist/t10k-images.csv..." >> hFlush stdout
    t10kImages  :: Tensor Double <- fromCSVFile "mnist/t10k-images.csv"  ',' "ij"
    lift $ putStrLn "Loading mnist/t10k-labels.csv..." >> hFlush stdout
    t10kLabels  :: Tensor Double <- fromCSVFile "mnist/t10k-labels.csv"  ',' "ij"
    lift $ learnPerceptron trainImages trainLabels t10kImages t10kLabels

learnPerceptron :: Tensor Double
                -> Tensor Double
                -> Tensor Double
                -> Tensor Double
                -> IO ()
learnPerceptron trainImages trainLabels t10kImages t10kLabels = do
    putStrLn ""
    putStrLn "Loaded CSV files. Learning perceptron..."
    putStrLn $ "Training images: " ++ show (imagesNum trainImages)
    hFlush stdout

    let trainResponses = mnistResponses trainLabels
    let t10kResponses  = mnistResponses t10kLabels

    w0 <- (Matrix.randomDouble "ij" 10 40 $ normalDistr 0.0 2.0)
    let p = perceptron trainImages trainResponses w0 ln
    let y = p `deepseq` sgn `Multilinear.Class.map` (p $| ("i","j") * t10kImages $| ("j","t"))
    

    putStrLn "y[0] = "
    hFlush stdout
    --mapM_ (\i -> print $ (y $| ("i","t")) $$| ("t",[i])) [0..9]
    print $ (y $| ("i","t")) $$| ("t",[0])
    putStrLn "expected = "
    print $ (t10kResponses $| ("i","t")) $$| ("t",[0])
    hFlush stdout


-- ENTRY POINT
main :: IO ()
main = do
    runExceptT getCSV
    return ()
    
