{-# LANGUAGE Unsafe #-}
{- |
Module: Blockchain.Fae.FrontEnd
Description: The API for implementors of a Fae front-end
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

If you are writing a Fae client, this module is for you.  It exposes the functions for running blocks and transactions, and for showing the storage.
-}
module FaeFrontend
  (
    -- * Transaction evaluation
    module FaeTXSummary,

    module FaeTypes,
    -- * Cryptography types and functions
    module FaeCrypto,
  ) where

import FaeTypes

import FaeCrypto hiding
  (
    Serialize, PassFail, PartialSerialize,
    compareSerialize, putPartialSerialize, 
    getPartialSerialize, readsPrecSer,
    EdPublicKey
  )
import FaeTXSummary
  (
    TXSummary(..), TXInputSummary(..), 
    InputSummary, InputSummaries, MaterialsSummaries,
  )
