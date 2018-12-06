module Test.Sequential (
  writeScalarCSV, readScalarCSV,
  writeVectorCSV, readVectorCSV,
  writeFormCSV, readFormCSV,
  writeMatrixCSV, readMatrixCSV
) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.Maybe
import           Multilinear.Class
import           Multilinear.Generic.Sequential
import           Multilinear.Generic.Sequential.Serialize
import           Multilinear.Index

writeScalarCSV :: Tensor Double -> String -> IO ()
writeScalarCSV m fileName = do
  putStrLn $ "Writing scalar to " ++ fileName ++ ".csv..."
  m `toCSVFile` (fileName ++ ".csv")
  putStrLn "Scalar successfully written!"

readScalarCSV :: Tensor Double -> String -> ExceptT String IO ()
readScalarCSV m fileName = do
  lift $ putStrLn $ "Reading scalar s1 from " ++ fileName ++ ".csv..."
  (m1' :: Tensor Double) <- fromCSVFile (fileName ++ ".csv") ',' "ij"
  if m1' == m then
    lift $ putStrLn "Scalar successfully read!"
  else
    ExceptT $ return $ Left "Scalar deserialization error"

writeVectorCSV :: Tensor Double -> String -> IO ()
writeVectorCSV m fileName = do
  let size = fromJust . indexSize <$> head $ indices m
  putStrLn $ "Writing " ++ show size ++ " elements vector to " ++ fileName ++ ".csv..."
  m `toCSVFile` (fileName ++ ".csv")
  putStrLn "Vector successfully written!"

readVectorCSV :: Tensor Double -> String -> ExceptT String IO ()
readVectorCSV m fileName = do
  lift $ putStrLn $ "Reading vector v1 from " ++ fileName ++ ".csv..."
  (m1' :: Tensor Double) <- fromCSVFile (fileName ++ ".csv") ',' "ij"
  if m1' == m then
    lift $ putStrLn "Vector successfully read!"
  else
    ExceptT $ return $ Left "Vector deserialization error"

writeFormCSV :: Tensor Double -> String -> IO ()
writeFormCSV m fileName = do
  let size = fromJust . indexSize <$> head $ indices m
  putStrLn $ "Writing " ++ show size ++ " elements form to " ++ fileName ++ ".csv..."
  m `toCSVFile` (fileName ++ ".csv")
  putStrLn "Form successfully written!"

readFormCSV :: Tensor Double -> String -> ExceptT String IO ()
readFormCSV m fileName = do
  lift $ putStrLn $ "Reading form f1 from " ++ fileName ++ ".csv..."
  (m1' :: Tensor Double) <- fromCSVFile (fileName ++ ".csv") ',' "ij"
  if m1' == m then
    lift $ putStrLn "Form successfully read!"
  else
    ExceptT $ return $ Left "Form deserialization error"

writeMatrixCSV :: Tensor Double -> String -> IO ()
writeMatrixCSV m fileName = do
  let [xsize,ysize] = indexSize <$> indices m
  putStrLn $ "Writing " ++ show (fromJust xsize) ++ "x" ++ show (fromJust ysize) ++ " matrix to " ++ fileName ++ ".csv..."
  m `toCSVFile` (fileName ++ ".csv")
  putStrLn "Matrix successfully written!"

readMatrixCSV :: Tensor Double -> String -> ExceptT String IO ()
readMatrixCSV m fileName = do
  lift $ putStrLn $ "Reading matrix m1 from " ++ fileName ++ ".csv..."
  (m1' :: Tensor Double) <- fromCSVFile (fileName ++ ".csv") ',' "ij"
  if m1' == m then
    lift $ putStrLn "Matrix successfully read!"
  else
    ExceptT $ return $ Left "Matrix deserialization error"
