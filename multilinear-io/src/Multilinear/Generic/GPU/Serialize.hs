{-|
Module      : Multilinear.Generic.GPU.Serialize
Description : Generic array tensor serialization: binary, JSON, CSV. 
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

-}

module Multilinear.Generic.GPU.Serialize (
    toBinary, toBinaryFile,
    fromBinary, fromBinaryFile,
    Multilinear.Generic.GPU.Serialize.toJSON, 
    Multilinear.Generic.GPU.Serialize.toJSONFile,
    Multilinear.Generic.GPU.Serialize.fromJSON, 
    Multilinear.Generic.GPU.Serialize.fromJSONFile,
    fromCSVFile, toCSVFile
) where

import           Codec.Compression.GZip
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import qualified Data.ByteString.Lazy                    as ByteString
import           Data.Conduit
import           Data.Conduit.Combinators
import           Data.Csv
import           Data.Serialize
import           Data.Vector.Serialize      ()
import qualified Data.Vector.Unboxed                     as Unboxed
import           Foreign.Storable
import qualified Multilinear.Generic                     as Generic
import           Multilinear.Generic.GPU
import qualified Multilinear.Generic.MultiCore.Serialize as MultiCore
import           Multilinear.Index.Finite.Serialize ()

-- Binary serialization instance
instance (Serialize a, Unboxed.Unbox a, Storable a) => Serialize (Tensor a)
-- JSON serialization instance
instance (ToJSON a, Unboxed.Unbox a, Storable a) => ToJSON (Tensor a)
-- JSON deserialization instance
instance (FromJSON a, Unboxed.Unbox a, Storable a) => FromJSON (Tensor a)

{-| Serialize tensor to zlib compressed binary string -}
toBinary :: (
    Serialize a, Unboxed.Unbox a, Storable a
  ) => Tensor a              -- ^ Tensor to serialize
    -> ByteString.ByteString -- ^ Tensor serialized to lazy ByteString
toBinary = compress . Data.Serialize.encodeLazy

{-| Write tensor to binary file. Uses compression with gzip -}
toBinaryFile :: (
    Serialize a, Unboxed.Unbox a, Storable a
  ) => Tensor a  -- ^ Tensor to serialize
    -> String    -- ^ File name
    -> IO ()
toBinaryFile t fileName = do
  let bs = toBinary t
  runConduitRes $ 
    sourceLazy bs .| sinkFile fileName

{-| Deserialize tensor from zlib compressed binary string -}
fromBinary :: (
    Serialize a, Unboxed.Unbox a, Storable a
  ) => ByteString.ByteString    -- ^ ByteString to deserialize
    -> Either String (Tensor a) -- ^ Deserialized tensor or an error message. 
fromBinary = Data.Serialize.decodeLazy . decompress

{-| Read tensor from binary file -}
fromBinaryFile :: (
    Serialize a, Unboxed.Unbox a, Storable a
  ) => String                       -- ^ File path. 
    -> ExceptT String IO (Tensor a) -- ^ Deserialized tensor or an error message
fromBinaryFile fileName = do
  contents <- lift $ runConduitRes $ 
    sourceFile fileName .| sinkLazy
  ExceptT $ return $ fromBinary contents

{-| Serialize tensor to JSON string -}
toJSON :: (
    ToJSON a, Unboxed.Unbox a, Storable a
  ) => Tensor a              -- ^ Tensor to serialize. 
    -> ByteString.ByteString -- ^ Tensor serialized to lazy ByteString. 
toJSON = Data.Aeson.encode

{-| Write tensor to JSON file -}
toJSONFile :: (
    ToJSON a, Unboxed.Unbox a, Storable a
  ) => Tensor a -- ^ Tensor to serialize
    -> String   -- ^ File path. 
    -> IO ()
toJSONFile t fileName = do
  let bs = Multilinear.Generic.GPU.Serialize.toJSON t
  runConduitRes $ 
    sourceLazy bs .| sinkFile fileName

{-| Deserialize tensor from JSON string -}
fromJSON :: (
    FromJSON a, Unboxed.Unbox a, Storable a
  ) => ByteString.ByteString -- ^ ByteString to deserialize
    -> Maybe (Tensor a)      -- ^ Deserialized tensor or Nothing, if deserialization error occured. 
fromJSON = Data.Aeson.decode

{-| Read tensor from JSON file -}
fromJSONFile :: (
    FromJSON a, Unboxed.Unbox a, Storable a
  ) => String               -- ^ File path. 
    -> MaybeT IO (Tensor a) -- ^ Deserialized tensor or Nothing, if error occured. 
fromJSONFile fileName = do
  contents <- lift $ runConduitRes $ 
    sourceFile fileName .| sinkLazy
  MaybeT $ return $ Multilinear.Generic.GPU.Serialize.fromJSON contents

{-| Write tensor to CSV file -}
toCSVFile :: (
  ToField a, Unboxed.Unbox a, Storable a
  ) => Tensor a -- ^ Tensor to serialize
    -> String   -- ^ File path
    -> IO ()

-- Encoding Scalar value
toCSVFile t = MultiCore.toCSVFile (Generic.gpuToMultiCore t)

{-| Read tensor from CSV -}
fromCSVFile :: (
  FromField a, Unboxed.Unbox a, Storable a
  ) => String -- ^ File path. 
    -> Char   -- ^ CSV separator
    -> String -- ^ Matrix indices names
    -> ExceptT String IO (Tensor a)  -- ^ Deserialized tensor or an error message
fromCSVFile path sep inames = do
    t <- MultiCore.fromCSVFile path sep inames
    return $ Generic.multiCoreToGPU t
