module Test.MultiCore (
  writeMatrixBinary, readMatrixBinary
) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.Maybe
import           Multilinear.Class
import           Multilinear.Generic.MultiCore
import           Multilinear.Generic.MultiCore.Serialize
import           Multilinear.Index

writeMatrixBinary :: Tensor Double -> String -> IO ()
writeMatrixBinary m fileName = do
  let [xsize,ysize] = indexSize <$> indices m
  putStrLn $ "Writing " ++ show (fromJust xsize) ++ "x" ++ show (fromJust ysize) ++ " matrix to " ++ fileName ++ ".zlib..."
  m `toBinaryFile` (fileName ++ ".zlib")
  putStrLn $ "Matrix successfully written!"

readMatrixBinary :: Tensor Double -> String -> ExceptT String IO ()
readMatrixBinary m fileName = do
  lift $ putStrLn $ "Reading matrix m1 from " ++ fileName ++ ".zlib..."
  m1' <- fromBinaryFile (fileName ++ ".zlib")
  if m1' == m then
    lift $ putStrLn "Matrix successfully read!"
  else
    ExceptT $ return $ Left "Matrix deserialization error"
