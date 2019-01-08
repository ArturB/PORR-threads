module PORR.Perceptron.GPU (
    imagesNum, pcaNum, perceptron, sgn, 
    learnBinaryPerceptron, testBinaryPerceptron,
    commiteeAnswer, commiteeAnswers, commiteeAccuracy
) where

import qualified Control.Parallel.Strategies   as Parallel
import           Data.Foldable
import           Data.Maybe
import           Foreign.ForeignPtr
import           Foreign.ForeignPtr.Unsafe
import           Foreign.Ptr
import           Multilinear.Class
import           Multilinear.Generic.GPU       as GPU
import           Multilinear.Index
import qualified Multilinear.Index.Finite      as Finite
import qualified Multilinear.Form              as Form
import qualified Multilinear.Vector            as Vector
import qualified Data.Vector.Storable          as StorableV
import           System.IO.Unsafe

foreign import ccall "matrixMult" 
    matrixMult :: 
        Ptr Double -- ^ First matrix to mult
     -> Ptr Double -- ^ Second matrix to mult
     -> Int        -- ^ Rows1
     -> Int        -- ^ Cols1
     -> Int        -- ^ Rows2
     -> Int        -- ^ Cols2
     -> Ptr Double -- ^ Multiplication result in serialized format

foreign import ccall unsafe "stdlib.h &free" c_free_ptr :: FinalizerPtr Double

c_mmult :: 
    StorableV.Vector Double -- ^ First matrix to mult
 -> StorableV.Vector Double -- ^ Second matrix to mult
 -> [TIndex]                -- ^ indices of first matrix
 -> [TIndex]                -- ^ indices of second matrix
 -> StorableV.Vector Double -- ^ result matrix
c_mmult m1 m2 i1 i2 = let
    inds1 = Finite.fromTIndex <$> i1
    inds2 = Finite.fromTIndex <$> i2
    (fp1, _, _) = StorableV.unsafeToForeignPtr m1
    (fp2, _, _) = StorableV.unsafeToForeignPtr m2
    p1 = unsafeForeignPtrToPtr fp1
    p2 = unsafeForeignPtrToPtr fp2
    rows1 = Finite.indexSize $ head inds1
    cols1 = Finite.indexSize $ inds1 !! 1
    rows2 = Finite.indexSize $ head inds2
    cols2 = Finite.indexSize $ inds2 !! 1
    result = matrixMult p1 p2 rows1 cols1 rows2 cols2
    fpResult = unsafePerformIO $ newForeignPtr c_free_ptr result
    in StorableV.unsafeFromForeignPtr0 fpResult (rows1 * cols2)


-- | Binary (0,1) signum function
sgn :: (Num a, Ord a) => a -> a
sgn x = if x > 0 then 1 else 0

rsgn :: (Num a, Ord a) => a -> a
rsgn x = if x < 0 then 1 else 0

-- | Return number of images (indexed by lower index) in given data
imagesNum :: Tensor Double -> Int
imagesNum t = 
    let t' = (t $| ("i","t")) <<<| "t"
    in  fromJust $ indexSize $ head $ indices t'

-- | Return number of PCA components in given data
pcaNum :: Tensor Double -> Int
pcaNum t = 
    let t' = (t $| ("i","t")) <<<| "i"
    in  fromJust $ indexSize $ head $ indices t'

-- | calculate weights of perceptron in next learning step
nextWeights :: Tensor Double           -- ^ positive class inputs tensor
            -> StorableV.Vector Double -- ^ positive class inputs
            -> Tensor Double           -- ^ negative class inputs tensor
            -> StorableV.Vector Double -- ^ negative class inputs
            -> [TIndex]                -- ^ positive class indices
            -> [TIndex]                -- ^ negative class indices
            -> Int                     -- ^ PosNum
            -> Int                     -- ^ NegNum
            -> Tensor Double -- ^ current weights
            -> Tensor Double -- ^ next weights
nextWeights xpos xposV xneg xnegV indsPos indsNeg posNum negNum _w  = 
    let w = _w $| ("","i")
        wInds = Multilinear.Index.Contravariant (Just 1) "a" : indices w
        wV = GPU.toVector w
        wXposV = c_mmult wV xposV wInds indsPos
        wXpos = GPU.fromVector (tail indsPos) wXposV
        wXnegV = c_mmult wV xnegV wInds indsNeg
        wXneg = GPU.fromVector (tail indsNeg) wXnegV

        ypos = rsgn `GPU.map` wXpos -- y $| ("","t")
        yneg = rsgn `GPU.map` wXneg -- y $| ("","t")
        incWpos = (ypos * xpos \/ "i") * Vector.const "t" posNum 1.0
        incWneg = (yneg * xneg \/ "i") * Vector.const "t" negNum 1.0
    in  w + incWpos + incWneg

-- | learn perceptron with given images and given number of learning iterations
perceptron :: Tensor Double -- ^ Positive samples
           -> Tensor Double -- ^ Negative samples
           -> Tensor Double -- ^ Initial weights
           -> Int           -- ^ Number of learning iterations
           -> Tensor Double -- ^ Trained weights
perceptron pos neg = let 
    xpos = pos $| ("i","t")
    xneg = (-1) *. ( neg $| ("i","t") )
    posNum = (imagesNum xpos)
    negNum = imagesNum xneg
    vPos = GPU.toVector $ Multilinear.Class.standardize xpos
    vNeg = GPU.toVector $ Multilinear.Class.standardize xneg
    apply f x n = 
        if n == 0 then x else apply f (f x) (n - 1)
    in apply (nextWeights xpos vPos xneg vNeg (indices pos) (indices neg) posNum negNum)
  

-- | Learn binary perceptron with given training data
learnBinaryPerceptron :: 
    Tensor Double -- ^ training images
 -> Tensor Double -- ^ training labels
 -> (Int,Int)     -- ^ Positive and negative class digit
 -> Int           -- ^ Number of learning iterations
 -> Tensor Double 
learnBinaryPerceptron trainImages' trainLabels' (pos,neg) learnIters =
        -- from all MNIST images and labels, filter only required 
    let trainPositiveImages = Multilinear.Class.filterIndex "t" 
            (\i -> (trainLabels' $$| ("t",[i])) == Scalar (fromIntegral pos) ) trainImages'
        trainNegativeImages = Multilinear.Class.filterIndex "t" 
            (\i -> (trainLabels' $$| ("t",[i])) == Scalar (fromIntegral neg) ) trainImages'
        w0 = Form.const "i" (pcaNum trainImages') 1.0
    in  perceptron trainPositiveImages trainNegativeImages w0 learnIters

-- | Test binary perceptron with given testing data and labels
testBinaryPerceptron ::
    Tensor Double -- ^ perceptron to test
 -> Tensor Double -- ^ testing samples
 -> Tensor Double -- ^ testing labels
 -> (Int,Int)     -- ^ Positive and negative class digit
 -> Double        -- ^ Perceptron accuracy in %
testBinaryPerceptron p testImages' testLabels' (pos,neg) = 
    let -- from all MNIST images and labels, filter only required  
        positiveImages = Multilinear.Class.filterIndex "t" 
            (\i -> (testLabels' $$| ("t",[i])) == Scalar (fromIntegral pos) ) testImages'
        negativeImages = Multilinear.Class.filterIndex "t" 
            (\i -> (testLabels' $$| ("t",[i])) == Scalar (fromIntegral neg) ) testImages'
        -- nums
        posNum = imagesNum positiveImages
        negNum = imagesNum negativeImages
        -- calculate output and accuracy
        ypos = sgn `GPU.map` (p $| ("","j") * positiveImages $| ("j","t"))
        yneg = sgn `GPU.map` (p $| ("","j") * negativeImages $| ("j","t"))
        posValid = ypos * Vector.const "t" posNum 1.0
        negValid = yneg * Vector.const "t" negNum 1.0
        accuracy = (scalarVal posValid + scalarVal negValid) / (fromIntegral posNum + fromIntegral negNum)
    in  fromIntegral (floor (1000 * accuracy)) / 10.0
    --in error $ show (order p) ++ ", " ++ show (order positiveImages)

-- | Given a binary perceptron commitee, try to classify the input using majority voting
commiteeAnswer :: 
    [(Int,Int,Tensor Double)] -- ^ Binary perceptrons commitee
 -> Tensor Double             -- ^ Inputs to classify - one digit per column
 -> Double                    -- ^ commitee answers
commiteeAnswer comm inp = 
    let binaryAnswers = fmap ( \(pos,neg,perc) -> 
                            ( pos, neg, scalarVal $ perc $| ("","i") * inp $| ("i","") ) 
                        ) comm
        noVotes pos = foldr'
                        (\(pos',neg',ans') ans2 -> 
                            if (pos == pos' && ans' > 0.0) || (pos == neg' && ans' < 0.0) then 
                                ans2 + 1.0 
                            else ans2
                        ) 0.0 binaryAnswers
        classVotes = fmap (\pos -> (pos, noVotes pos)) [0..9]
        (maxClass,_) = foldr' 
                       (\(pos,ans) (maxPos,maxAns) -> if ans > maxAns then (pos,ans) else (maxPos,maxAns)) 
                       (-1,-1) classVotes
    in  fromIntegral maxClass

-- | Answers of perceptron commitee on set of input examples
commiteeAnswers :: 
    [(Int,Int,Tensor Double)] -- ^ Binary perceptrons commitee
 -> Tensor Double             -- ^ Inputs to classify - one digit per column
 -> Tensor Double             -- ^ commitee answers
commiteeAnswers comm testImages = 
    let iNum = imagesNum testImages
        testAnswers = (\i -> commiteeAnswer comm (_mergeScalars (testImages $$| ("t",[i]))) ) <$> [0 .. iNum - 1]
        testAnswers' = testAnswers `Parallel.using` Parallel.parListChunk (iNum `div` 8) Parallel.rdeepseq
    in  Form.fromIndices "t" iNum (testAnswers' !!)

-- | Calculate commitee accuracy
commiteeAccuracy ::
    [(Int,Int,Tensor Double)] -- ^ Binary perceptrons commitee
 -> Tensor Double             -- ^ Inputs to classify - one digit per column
 -> Tensor Double             -- ^ Labels
 -> Double                    -- ^ Accuracy
commiteeAccuracy comm testImages testLabels = 
    let imgNo = imagesNum testImages
        commAnswers = commiteeAnswers comm testImages
        -- check with referential labels
        validAnswers = sum $ fmap
            (\i -> if testLabels $$| ("t",[i]) == commAnswers $$| ("t",[i]) then 1 else 0)
            [0.. imgNo - 1]
        -- calculate and print perceptron accuracy
        accuracy = validAnswers / fromIntegral imgNo
    in  fromIntegral (floor (1000 * accuracy)) / 10
