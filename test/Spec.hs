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
import           Data.Foldable

nextWeights :: Tensor Double   -- ^ current weights
            -> (Tensor Double, -- ^ current input
                Tensor Double) -- ^ expected output
            -> Tensor Double   -- ^ next weights
nextWeights w (x,e) = 
    let y = signum $ w $| ("i","j") * x $| ("j","")
        d = e $| ("i","") - y $| ("i","")
    in  w $| ("i","j") + x $| ("i","") * d $| ("","j")

perceptron :: Tensor Double -- ^ Training images
           -> Tensor Double -- ^ Training labels
           -> Tensor Double -- ^ Trained weights
perceptron ts es =
    let w0 = Matrix.const "ij" 10 40 0
        trainingVector = Boxed.generate 60000 $ \i -> (ts $| ("i","t") )$$| ("t",[i])
        labelVector = Boxed.generate 60000 $ \i -> (es $| ("i","t")) $$| ("t",[i])
        imagesWithLabels = Boxed.zip trainingVector labelVector
    in  foldl' nextWeights w0 imagesWithLabels


getCSV :: ExceptT String IO ()
getCSV = do
    trainImages :: Tensor Double <- fromCSVFile "mnist/train-images.csv" ',' "ij"
    labelImages  :: Tensor Double <- fromCSVFile "mnist/train-labels.csv" ',' "ij"

    t10kImages  :: Tensor Double <- fromCSVFile "mnist/t10k-images.csv" ',' "ij"
    l10kImages  :: Tensor Double <- fromCSVFile "mnist/t10k-labels.csv" ',' "ij"

    let trainResponses = (\e -> Vector.fromIndices "i" 10 $ \x -> if fromIntegral x == e then 1.0 else 0.0) <$> labelImages
    let t10kResponses = (\e -> Vector.fromIndices "i" 10 $ \x ->if fromIntegral x == e then 1.0 else 0.0) <$> l10kImages

    let p = perceptron trainImages trainResponses
    let y0 = p $| ("i","j") * t10kImages $| ("j","t")

    lift $ print "\n"
    lift $ print $ (y0 $| ("i","t")) $$| ("t",[0])

-- ENTRY POINT
main :: IO ()
main = do
  runExceptT getCSV
  return ()
  
