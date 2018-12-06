module Main (
  main
) where

import           Control.Monad.Trans.Except
import qualified Multilinear.Generic.MultiCore  as MultiCore
import qualified Multilinear.Generic.Sequential as Sequential
import qualified Multilinear.Generic.GPU        as GPU
import qualified Multilinear.Matrix             as Matrix
import           System.Directory
import           Test.MultiCore
import           Test.Sequential
import           Test.GPU

fileName1 :: String
fileName1  = "test/m1"

m1Seq :: Sequential.Tensor Double
m1Seq = Matrix.fromIndices "ij" 50 50 $ \i j -> cos (fromIntegral i) + sin (fromIntegral j)

m1MC :: MultiCore.Tensor Double
m1MC = Matrix.fromIndices "ij" 50 50 $ \i j -> cos (fromIntegral i) + sin (fromIntegral j)

m1GPU :: GPU.Tensor Double
m1GPU = Matrix.fromIndices "ij" 50 50 $ \i j -> cos (fromIntegral i) + sin (fromIntegral j)

-- ENTRY POINT
main :: IO ()
main = do
  putStrLn "Testing MultiCore..."
  Test.MultiCore.writeMatrixBinary m1MC fileName1
  runExceptT $ Test.MultiCore.readMatrixBinary m1MC fileName1
  removeFile $ fileName1 ++ ".zlib"
  putStrLn "Testing Sequential..."
  Test.Sequential.writeMatrixBinary m1Seq fileName1
  runExceptT $ Test.Sequential.readMatrixBinary m1Seq fileName1
  removeFile $ fileName1 ++ ".zlib"
  putStrLn "Testing GPU..."
  Test.GPU.writeMatrixBinary m1GPU fileName1
  runExceptT $ Test.GPU.readMatrixBinary m1GPU fileName1
  removeFile $ fileName1 ++ ".zlib"
  return ()
