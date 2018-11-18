module Perceptron (
    perceptron, sgn
) where

--import           Control.DeepSeq
import           Data.Maybe
import           Multilinear
import           Multilinear.Generic
import           Multilinear.Index
import qualified Multilinear.Vector            as Vector

-- | Binary (0,1) signum function
sgn :: (Num a, Ord a) => a -> a
sgn x = if x > 0 then 1 else 0

-- | calculate weights of perceptron in next learning step
nextWeights :: Tensor Double -- ^ actual inputs
            -> Tensor Double -- ^ expected outputs
            -> Tensor Double -- ^ current weights
            -> Tensor Double -- ^ next weights
nextWeights _x _e _w  = 
    let exNum = fromJust $ indexSize $ indices x !! 1
        x = _x $| ("i","t")
        e = _e $| ("n","t")
        w = _w $| ("n","i")
        y = sgn `Multilinear.map` w * x -- y $| ("n","t")
        d = e - y -- d $| ("n","t")
        incW = x \/ "i" * d * Vector.const "t" exNum 1.0 -- incW $| ("n","i")
    in  w + incW

-- | learn perceptron with given images and given number of learning iterations
perceptron :: Tensor Double -- ^ Training images
           -> Tensor Double -- ^ Training labels
           -> Tensor Double -- ^ Initial weights
           -> Int           -- ^ Number of learning iterations
           -> Tensor Double -- ^ Trained weights
perceptron ts es w0 n = iterate (nextWeights ts es) w0 !! n
