{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module FaeTXSummary where
{-
import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Monitors
import Blockchain.Fae.Internal.Storage
import Blockchain.Fae.Internal.TX
-}

import FaeCrypto
import FaeTypes

import Control.Lens

import Control.DeepSeq

import System.IO.Unsafe

import Control.DeepSeq
import Control.Monad
import Control.Monad.State

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import Data.Vector (Vector)

import Data.Foldable

import GHC.Generics hiding (to)

data Status = Updated | Deleted | Failed deriving (Show, Generic)

-- | Useful for Fae clients communicating with faeServer
data TXSummary = TXSummary {
  transactionID :: TransactionID,
  txResult :: String,
  txOutputs:: Vector VersionID,
  txInputSummaries :: InputSummaries,
  txMaterialsSummaries :: MaterialsSummaries,
  txSSigners :: [(String, PublicKey)]
} deriving (Generic)

data TXInputSummary = TXInputSummary {
  txInputStatus :: Status,
  txInputOutputs :: Vector VersionID,
  txInputMaterialsSummaries :: MaterialsSummaries,
  txInputVersion :: VersionID
} deriving (Generic)

type InputSummary = (ContractID, TXInputSummary)
type InputSummaries = Vector InputSummary
type MaterialsSummaries = Vector (String, InputSummary)
