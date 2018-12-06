module Test.MultiCore (
  writeMatrixJSON, readMatrixJSON
) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Maybe
import           Multilinear.Class
import           Multilinear.Generic.MultiCore
import           Multilinear.Generic.MultiCore.Serialize
import           Multilinear.Index

writeMatrixJSON :: Tensor Double -> String -> IO ()
writeMatrixJSON m fileName = do
  let [xsize,ysize] = indexSize <$> indices m
  putStrLn $ "Writing " ++ show (fromJust xsize) ++ "x" ++ show (fromJust ysize) ++ " matrix to " ++ fileName ++ ".json..."
  m `toJSONFile` (fileName ++ ".json")
  putStrLn $ "Matrix successfully written!"

readMatrixJSON :: Tensor Double -> String -> MaybeT IO ()
readMatrixJSON m fileName = do
  lift $ putStrLn $ "Reading matrix m1 from " ++ fileName ++ ".json..."
  m1' <- fromJSONFile (fileName ++ ".json")
  if m1' == m then
    lift $ putStrLn "Matrix successfully read!"
  else
    MaybeT $ return Nothing
