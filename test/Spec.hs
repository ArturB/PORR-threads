module Main (
    main
) where

import           Control.Exception.Base
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Class
import           Multilinear.Class
import           Multilinear.Generic
import           Multilinear.Generic.Serialize

getCSV :: ExceptT String IO ()
getCSV = do
    trainImages :: Tensor Double <- fromCSVFile "mnist/train-images.csv" ',' "ij"
    t10kImages  :: Tensor Double <- fromCSVFile "mnist/t10k-images.csv" ',' "ij"
    lift $ print "\n"
    lift $ putStrLn $ "train-images indices: " ++ show (indices trainImages)
    lift $ putStrLn $ "t10k-images indices: "  ++ show (indices t10kImages)

-- ENTRY POINT
main :: IO ()
main = do
  runExceptT getCSV
  return ()
  
