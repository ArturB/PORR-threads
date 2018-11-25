module Main (
    main
) where
{-
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Class
import           Data.Foldable
import           Data.Maybe
import           Multilinear.Class
import           Multilinear.Generic.MultiCore
import           Multilinear.Generic.Serialize
import           Multilinear.Index
import qualified Multilinear.Form              as Form
import qualified Multilinear.Matrix            as Matrix
import           Perceptron
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
    in  (\x -> if x `elem` ps' then 1.0 else 0.0) `Multilinear.Generic.MultiCore.map` ls

-- | Return number of images (indexed by lower index) in given data
imagesNum :: Tensor Double -> Int
imagesNum t = 
    let t' = (t $| ("i","t")) <<<| "t"
    in  fromJust $ indexSize $ head $ indices t'

-- | Learn binary perceptron with given training data
learnBinaryPerceptron :: 
    Tensor Double -- ^ training images
 -> Tensor Double -- ^ training labels
 -> (Int,Int)     -- ^ Positive and negative class digit
 -> Tensor Double 
learnBinaryPerceptron trainImages' trainLabels' (pos,neg) =
        -- from all MNIST images and labels, filter only required 
    let trainImages = filterIndex "t" 
            (\i -> (trainLabels' $$| ("t",[i])) `elem` [Scalar $ fromIntegral pos, Scalar $ fromIntegral neg] ) trainImages'
        trainLabels = filterIndex "t" 
            (\i -> (trainLabels' $$| ("t",[i])) `elem` [Scalar $ fromIntegral pos, Scalar $ fromIntegral neg] ) trainLabels'
        -- train perceptron and return
        trainResponses = mnistBinaryResponses trainLabels [pos]
        w0 = Matrix.const "ij" 1 40 0.0
    in  perceptron trainImages trainResponses w0 learnIters

-- | Test binary perceptron with given testing data and labels
testBinaryPerceptron ::
    Tensor Double -- ^ perceptron to test
 -> Tensor Double -- ^ testing images
 -> Tensor Double -- ^ testing labels
 -> (Int,Int)     -- ^ Positive and negative class digit
 -> Double        -- ^ Perceptron accuracy 
testBinaryPerceptron p testImages' testLabels' (pos,neg) = 
    let -- from all MNIST images and labels, filter only required  
        testImages = filterIndex "t" 
            (\i -> (testLabels' $$| ("t",[i])) `elem` [Scalar $ fromIntegral pos, Scalar $ fromIntegral neg] ) testImages'
        testLabels = filterIndex "t" 
            (\i -> (testLabels' $$| ("t",[i])) `elem` [Scalar $ fromIntegral pos, Scalar $ fromIntegral neg] ) testLabels'
        -- calculate output and accuracy
        y = sgn `Multilinear.Generic.MultiCore.map` (p $| ("i","j") * testImages $| ("j","t"))
        expectedResponses  = mnistBinaryResponses testLabels [pos]
        nT10k = testLabels `Multilinear.Class.size` "t"
    in  sum ((\i -> 
            if y $| ("i","t") $$| ("it",[0,i]) == expectedResponses $| ("i","t") $$| ("t",[i]) then 
                1.0 
            else 0.0
            ) <$> [0 .. nT10k - 1]) / fromIntegral nT10k

-- | Given a binary perceptron commitee, try to classify the input using majority voting
commiteeAnswer :: 
    [(Int,Int,Tensor Double)] -- ^ Binary perceptrons commitee
 -> Tensor Double             -- ^ Inputs to classify - one digit per column
 -> Double                    -- ^ commitee answers
commiteeAnswer comm inp = 
    let binaryAnswers = ( \(pos,neg,perc) -> ( pos, neg, Multilinear.Generic.MultiCore.map sgn (perc $| ("n","i") * inp $| ("i","") $$| ("n",[0])) ) ) <$> comm
        noVotes pos = foldr'
                        (\(pos',neg',ans') ans2 -> 
                            if (pos == pos' && ans' == 1.0) || (pos == neg' && ans' == 0.0) then 
                                ans2 + 1.0 
                            else ans2
                        ) 0.0 binaryAnswers
        classVotes = (\pos -> (pos, noVotes pos)) <$> [0..9]
        (maxClass,_) = 
            foldr' (\(pos,ans) (maxPos,maxAns) -> if ans > maxAns then (pos,ans) else (maxPos,maxAns)) 
                   (-1,-1) classVotes
    in  fromIntegral maxClass

commiteeAnswers :: 
    [(Int,Int,Tensor Double)] -- ^ Binary perceptrons commitee
 -> Tensor Double         -- ^ Inputs to classify - one digit per column
 -> Tensor Double            -- ^ commitee answers
commiteeAnswers comm testImages = 
    let iNum = imagesNum testImages
        testAnswers = (\i -> commiteeAnswer comm (_mergeScalars (testImages $$| ("t",[i]))) ) <$> [0 .. iNum - 1]
    in  Form.fromIndices "t" iNum (testAnswers !!)


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
    lift $ putStrLn $ "Loaded CSV files. Learning perceptron...\nIterations: " ++ show learnIters
    lift $ putStrLn $ "Training images: " ++ show (imagesNum trainImages)
    lift $ hFlush stdout
    -- generate 45 number pairs, according to 45 binary perceptrons commitee
    let numPairs = uncurry (<) `Prelude.filter` (pure (,) <*> [1..10] <*> [1..10])
    let commitee = (\(p,n) -> (p, n, learnBinaryPerceptron trainImages trainLabels' (p,n))) <$> numPairs
    -- get commitee responses for test images
    let commAnswers = commiteeAnswers commitee t10kImages
    -- check with referential labels
    let validAnswers = sum $ 
            (\i -> if t10kLabels' $$| ("t",[i]) == commAnswers $$| ("t",[i]) then 1 else 0) <$> [0..imagesNum t10kImages]
    -- calculate and print perceptron accuracy
    let accuracy = validAnswers / fromIntegral (imagesNum t10kImages)
    --lift $ print $ commiteeAnswer commitee (_mergeScalars $ t10kImages $$| ("t",[0]))
    --lift $ print $ _mergeScalars $ t10kLabels' $$| ("t",[0])
    lift $ putStrLn "\nBinary perceptrons commitee learned!"
    lift $ putStrLn $ "Accuracy = " ++ show (floor (1000 * accuracy) `div` 10) ++ "%"

-- | ENTRY POINT
main :: IO ()
main = do
    runExceptT getCSV
    return ()
    
-}
main :: IO ()
main = print "TODO"
