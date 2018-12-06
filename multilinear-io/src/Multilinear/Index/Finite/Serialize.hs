{-|
Module      : Multilinear.Index.Finite.Serialize
Description : Finite-dimensional tensor index serialization: binary, JSON. 
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

Finite-dimensional tensor index.

-}

module Multilinear.Index.Finite.Serialize (
) where

import           Data.Aeson
import           Data.Serialize
import           Multilinear.Index.Finite

{-| Binary serialization and deserialization |-}
instance Serialize Index

{-| Serialization to and from JSON |-}
instance FromJSON Index
instance   ToJSON Index
