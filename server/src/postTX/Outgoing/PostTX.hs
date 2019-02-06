{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PostTX.Outgoing.PostTX where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Data.List

import Prelude

import System.Exit
import System.IO
import System.Process
import System.Environment
import System.Directory

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D

import Data.Either
import Data.Maybe

import Debug.Trace

import PostTX.Types
import PostTX.Incoming.Types
import PostTX.Outgoing.FormatTX
import PostTX.Outgoing.Types
import SharedTypes

import FaeTXSummary

parseTXSummary :: Text -> Either String TXSummary
parseTXSummary jsonTxt = eitherDecode $ C.pack $ T.unpack jsonTxt

postTX ::
     AuctionTXin
  -> ExceptT PostTXError (ReaderT TXConfig IO) (TXSummary)
postTX tx = do
  stdout <- liftIO $ readProcess "stack" ["exec", "posttx", "Create","--","--json"] []
  liftIO $ putStrLn stdout
  either (throwError . TXSummaryParseFailed) return (parseTXSummary $ T.pack stdout)
  where
    PostTXOpts {..} = traceShow (getPostTXopts tx) (getPostTXopts tx)
    finalArgs = []
{-  (exitCode, stdOut, stdErr) <- liftIO $ readProcessWithExitCode cmd args []
  liftIO $ System.IO.putStrLn stdOut
  liftIO $ System.IO.putStrLn stdErr
  return (exitCode, stdOut, stdErr)
  where
    args = traceShow (getPostTXargs tx ++ ["--json"]) (getPostTXargs tx  ++ ["--json"])
    cmd = "stack exec postTX"
    -}

{-

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

-}

placeBid :: ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
placeBid = do
  config@(BidConfig key aucTXID coinTXID) <- ask
  TXSummary{..} <-
    postTX (BidTXin key aucTXID coinTXID)
  let hasWon = txResult == "You won!"
  return $ BidTX transactionID aucTXID coinTXID hasWon

createAuction :: ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
createAuction = do
  (CreateAuctionConfig key aucStartingValue maxBidCount) <- ask
  TXSummary{..} <- postTX (CreateAuctionTXin key)
  return $ AuctionCreatedTX transactionID aucStartingValue maxBidCount

getCoin :: ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
getCoin = do
  (GetCoinConfig key) <- ask
  TXSummary{..} <- postTX (GetCoinTXin key)
  return $ GetCoinTX transactionID

-- take the coins from an old cache destroy the cache and deposit the old coins + 1 new coin to a new cache
getMoreCoins :: ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
getMoreCoins = do
  (GetMoreCoinsConfig key coinTXID) <- ask
  TXSummary{..} <- postTX (GetMoreCoinsTXin key coinTXID)
  return $ GetMoreCoinsTX transactionID

withdraw :: ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
withdraw = do
  (WithdrawConfig key aucTXID) <- ask
  TXSummary{..} <- postTX (WithdrawTXin key aucTXID)
  return $ WithdrawTX transactionID
