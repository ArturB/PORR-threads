{-|
Module      : Multilinear.Generic.Serialize
Description : Generic array tensor serialization: binary, JSON, CSV. 
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

-}

module Multilinear.Generic.MultiCore.Serialize (
    toBinary, toBinaryFile,
    fromBinary, fromBinaryFile,
    Multilinear.Generic.MultiCore.Serialize.toJSON, 
    Multilinear.Generic.MultiCore.Serialize.toJSONFile,
    Multilinear.Generic.MultiCore.Serialize.fromJSON, 
    Multilinear.Generic.MultiCore.Serialize.fromJSONFile,
    fromCSVFile, toCSVFile
) where

import           Codec.Compression.GZip
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import qualified Data.ByteString.Internal   as BS
import qualified Data.ByteString.Lazy       as ByteString
import           Data.Conduit
import           Data.Conduit.Combinators
import           Data.Csv
import           Data.Either
import           Data.Serialize
import qualified Data.Vector                as Boxed
import           Data.Vector.Serialize      ()
import qualified Data.Vector.Unboxed        as Unboxed
import           Multilinear.Generic.MultiCore
import qualified Multilinear.Index.Finite   as Finite
import           Multilinear.Index.Finite.Serialize ()

-- Binary serialization instance
instance (Serialize a, Unboxed.Unbox a) => Serialize (Tensor a)
-- JSON serialization instance
instance (ToJSON a, Unboxed.Unbox a) => ToJSON (Tensor a)
-- JSON deserialization instance
instance (FromJSON a, Unboxed.Unbox a) => FromJSON (Tensor a)
-- CSV serialization instance
instance (ToField a, Unboxed.Unbox a) => ToRecord (Tensor a) where
  toRecord (Scalar x) = record [toField x]
  toRecord (SimpleFinite _ scalars) = toRecord scalars
  toRecord _ = error "Only 1-order tensor may be converted to record!"
instance (FromField a, Unboxed.Unbox a) => FromRecord (Tensor a) where
  parseRecord v = 
    if Boxed.length v == 1 then
      Scalar <$> v .! 0
    else
      error "non-scalar"


{-| Serialize tensor to zlib compressed binary string -}
toBinary :: (
    Serialize a, Unboxed.Unbox a
  ) => Tensor a              -- ^ Tensor to serialize
    -> ByteString.ByteString -- ^ Tensor serialized to lazy ByteString
toBinary = compress . Data.Serialize.encodeLazy

{-| Write tensor to binary file. Uses compression with gzip -}
toBinaryFile :: (
    Serialize a, Unboxed.Unbox a
  ) => Tensor a  -- ^ Tensor to serialize
    -> String    -- ^ File name
    -> IO ()
toBinaryFile t fileName = do
  let bs = toBinary t
  runConduitRes $ 
    sourceLazy bs .| sinkFile fileName

{-| Deserialize tensor from zlib compressed binary string -}
fromBinary :: (
    Serialize a, Unboxed.Unbox a
  ) => ByteString.ByteString    -- ^ ByteString to deserialize
    -> Either String (Tensor a) -- ^ Deserialized tensor or an error message. 
fromBinary = Data.Serialize.decodeLazy . decompress

{-| Read tensor from binary file -}
fromBinaryFile :: (
    Serialize a, Unboxed.Unbox a
  ) => String                       -- ^ File path. 
    -> ExceptT String IO (Tensor a) -- ^ Deserialized tensor or an error message
fromBinaryFile fileName = do
  contents <- lift $ runConduitRes $ 
    sourceFile fileName .| sinkLazy
  ExceptT $ return $ fromBinary contents

{-| Serialize tensor to JSON string -}
toJSON :: (
    ToJSON a, Unboxed.Unbox a
  ) => Tensor a              -- ^ Tensor to serialize. 
    -> ByteString.ByteString -- ^ Tensor serialized to lazy ByteString. 
toJSON = Data.Aeson.encode

{-| Write tensor to JSON file -}
toJSONFile :: (
    ToJSON a, Unboxed.Unbox a
  ) => Tensor a -- ^ Tensor to serialize
    -> String   -- ^ File path. 
    -> IO ()
toJSONFile t fileName = do
  let bs = Multilinear.Generic.MultiCore.Serialize.toJSON t
  runConduitRes $ 
    sourceLazy bs .| sinkFile fileName

{-| Deserialize tensor from JSON string -}
fromJSON :: (
    FromJSON a, Unboxed.Unbox a
  ) => ByteString.ByteString -- ^ ByteString to deserialize
    -> Maybe (Tensor a)      -- ^ Deserialized tensor or Nothing, if deserialization error occured. 
fromJSON = Data.Aeson.decode

{-| Read tensor from JSON file -}
fromJSONFile :: (
    FromJSON a, Unboxed.Unbox a
  ) => String               -- ^ File path. 
    -> MaybeT IO (Tensor a) -- ^ Deserialized tensor or Nothing, if error occured. 
fromJSONFile fileName = do
  contents <- lift $ runConduitRes $ 
    sourceFile fileName .| sinkLazy
  MaybeT $ return $ Multilinear.Generic.MultiCore.Serialize.fromJSON contents

{-| Write tensor to CSV file -}
toCSVFile :: (
  ToField a, Unboxed.Unbox a
  ) => Tensor a -- ^ Tensor to serialize
    -> String   -- ^ File path
    -> IO ()

-- Encoding Scalar value
toCSVFile s@(Scalar _) fileName = do
  let bs = Data.Csv.encode [s]
  runConduitRes $ 
    sourceLazy bs .| sinkFile fileName

-- Encoding SimpleFinite (1D) tensor
toCSVFile (SimpleFinite _ vs) fileName = do
  let rows = [vs]
  let bs = Data.Csv.encode rows
  runConduitRes $ 
    sourceLazy bs .| sinkFile fileName

-- Encoding FiniteTensor (many dimensions)
toCSVFile (FiniteTensor _ vrows) fileName = do
  let rows = Boxed.toList vrows
  let bs = Data.Csv.encode rows
  runConduitRes $ 
    sourceLazy bs .| sinkFile fileName


{-| Read tensor from CSV -}
fromCSVFile :: (
  FromField a, Unboxed.Unbox a
  ) => String -- ^ File path. 
    -> Char   -- ^ CSV separator
    -> String -- ^ Matrix indices names
    -> ExceptT String IO (Tensor a)  -- ^ Deserialized tensor or an error message
fromCSVFile fileName separator [conI,covI] = do
  --let csvSettings = CSVSettings separator Nothing
  contents <- lift $ runConduitRes $ 
    sourceFile fileName .| sinkLazy
  let decodedData = Data.Csv.decodeWith (DecodeOptions (BS.c2w separator)) NoHeader contents :: (FromField a, Unboxed.Unbox a) => Either String (Boxed.Vector (Unboxed.Vector a))
  if isLeft decodedData 
  then do
    let msg = fromLeft "" decodedData
    ExceptT $ return $ Left msg
  else do
    let components = fromRight Boxed.empty decodedData
    let rows = (\r -> SimpleFinite (Finite.Covariant (Unboxed.length r) [covI]) r) <$> components
    ExceptT $ return $ Right $ FiniteTensor (Finite.Contravariant (Boxed.length rows) [conI]) rows 
fromCSVFile _ _ _ = error "You must provide exactly two indices names!"
