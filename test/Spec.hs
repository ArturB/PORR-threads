module Main (
    main
) where

import           Control.Exception.Base
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Class
import           Multilinear.Class
import           Multilinear.Generic
import           Multilinear.Generic.Serialize
import qualified Multilinear.Vector as Vector
import qualified Multilinear.Matrix as Matrix
import qualified Data.Vector as Boxed
import qualified Multilinear.Tensor as Tensor
import           Data.Foldable
import           System.IO

nextWeights :: Tensor Double   -- ^ current weights
            -> (Tensor Double, -- ^ current input
                Tensor Double) -- ^ expected output
            -> Tensor Double   -- ^ next weights
nextWeights w (x,e) = 
    let y = signum $ w $| ("a","b") * x $| ("b","")
        d = (e $| ("c","")) - (y $| ("c",""))
    in  (w $| ("i","j")) + ( ( x $| ("j","") \/ "j") * (d $| ("i","")) )

perceptron :: Tensor Double
           -> Tensor Double -- ^ Training images
           -> Tensor Double -- ^ Training labels
           -> Tensor Double -- ^ Trained weights
perceptron w0 ts es =
    let trainingVector = Boxed.generate 6000 $ \i -> (ts $| ("i","t")) $$| ("t",[i])
        labelVector = Boxed.generate 6000 $ \i -> (es $| ("i","t")) $$| ("t",[i])
        imagesWithLabels = Boxed.zip trainingVector labelVector
    in  foldl' nextWeights w0 imagesWithLabels


getCSV :: ExceptT String IO ()
getCSV = do
    trainImages :: Tensor Double <- fromCSVFile "mnist/train-images.csv" ',' "ij"
    labelImages  :: Tensor Double <- fromCSVFile "mnist/train-labels.csv" ',' "ij"

    t10kImages  :: Tensor Double <- fromCSVFile "mnist/t10k-images.csv" ',' "ij"
    l10kImages  :: Tensor Double <- fromCSVFile "mnist/t10k-labels.csv" ',' "ij"

    lift $ print ""
    lift $ print "Loaded CSV files..." >> hFlush stdout

    let trainLabel t = scalarVal $ (labelImages $| ("i","t")) $$| ("it",[0,t])
    let t10kLabel t = scalarVal $ (l10kImages $| ("i","t")) $$| ("it",[0,t])

    let trainResponses = Tensor.generate ("",[]) ("t",[60000]) (\_ [t] -> Vector.fromIndices "i" 10 $ \x -> if fromIntegral x == trainLabel t then 1.0 else 0.0)
    let t10kResponses = Tensor.generate ("",[]) ("t",[10000]) (\_ [t] -> Vector.fromIndices "i" 10 $ \x -> if fromIntegral x == t10kLabel t then 1.0 else 0.0)

    let p = perceptron (Matrix.const "ij" 10 40 0) trainImages trainResponses
    let y0 = signum $ p $| ("i","j") * t10kImages $| ("j","t")

    lift $ print $ (y0 $| ("i","t")) $$| ("t",[0])

-- ENTRY POINT
main :: IO ()
main = do
  runExceptT getCSV
  return ()
  
