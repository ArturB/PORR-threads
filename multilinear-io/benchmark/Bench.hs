{-|
Module      : Bench
Description : Benchmark of Multilinear library
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

-}

module Main (
    main
) where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Criterion.Main
import           Multilinear.Generic
import           Multilinear.Generic.Serialize
import qualified Multilinear.Matrix         as Matrix
import           System.Directory

pathPref :: String
pathPref = "benchmark/matrix-"

gen :: Int -> Int -> Double
gen j k = sin (fromIntegral j) + cos (fromIntegral k)

writeMatrixBinaryBench :: Int -> Benchmark
writeMatrixBinaryBench s = 
    let path = pathPref ++ show s ++ ".zlib" in 
    bench (show s ++ "x" ++ show s) $ 
        nfIO $ toBinaryFile (Matrix.fromIndices "ij" s s gen) path

readMatrixBinaryBench :: Int -> Benchmark
readMatrixBinaryBench s = do
    let path = pathPref ++ show s ++ ".zlib"
    let tensorDoubleIOT = fromBinaryFile path :: ExceptT String IO (Tensor Double)
    bench (show s ++ "x" ++ show s) $ 
        nfIO $ runExceptT tensorDoubleIOT

writeMatrixJSONBench :: Int -> Benchmark
writeMatrixJSONBench s = 
    let path = pathPref ++ show s ++ ".json" in 
    bench (show s ++ "x" ++ show s) $ 
        nfIO $ toJSONFile (Matrix.fromIndices "ij" s s gen) path

readMatrixJSONBench :: Int -> Benchmark
readMatrixJSONBench s = do
    let path = pathPref ++ show s ++ ".json"
    let tensorDoubleIOT = fromJSONFile path :: MaybeT IO (Tensor Double)
    bench (show s ++ "x" ++ show s) $ 
        nfIO $ runMaybeT tensorDoubleIOT

writeMatrixCSVBench :: Int -> Benchmark
writeMatrixCSVBench s = 
    let path = pathPref ++ show s ++ ".csv" in 
    bench (show s ++ "x" ++ show s) $ 
        nfIO $ toCSVFile (Matrix.fromIndices "ij" s s gen) path

readMatrixCSVBench :: Int -> Benchmark
readMatrixCSVBench s = do
    let path = pathPref ++ show s ++ ".csv"
    let tensorDoubleIOT = fromCSVFile path ',' "ij" :: ExceptT String IO (Tensor Double)
    bench (show s ++ "x" ++ show s) $ 
        nfIO $ runExceptT tensorDoubleIOT

benchSizes :: [Int]
benchSizes = [100, 200, 400, 800, 1600]

-- ENTRY POINT
mainBench :: IO ()
mainBench = defaultMain [
    bgroup "matrix binary write" $ writeMatrixBinaryBench <$> benchSizes,
    bgroup "matrix binary read"  $ readMatrixBinaryBench  <$> benchSizes,
    bgroup "matrix JSON write"   $ writeMatrixJSONBench   <$> benchSizes,
    bgroup "matrix JSON read"    $ readMatrixJSONBench    <$> benchSizes,
    bgroup "matrix CSV write"    $ writeMatrixCSVBench    <$> benchSizes,
    bgroup "matrix CSV read"     $ readMatrixCSVBench     <$> benchSizes
    ]

main :: IO ()
main = do
    mainBench
    let pathsBinary = (\s -> pathPref ++ show s ++ ".zlib") <$> benchSizes
    let pathsJSON   = (\s -> pathPref ++ show s ++ ".json") <$> benchSizes 
    let pathsCSV    = (\s -> pathPref ++ show s ++ ".csv")  <$> benchSizes
    mapM_ removeFile (pathsBinary ++ pathsJSON ++ pathsCSV)
