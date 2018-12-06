{-|
Module      : Multilinear.Generic.Serialize
Description : Generic array tensor serialization: binary, JSON, CSV. 
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

-}

module Multilinear.Generic.Serialize (
    module DefaultTensorImplementation
) where

import Multilinear.Generic.Sequential.Serialize as DefaultTensorImplementation
