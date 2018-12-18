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
import PostTX.Incoming.ParseTX
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

bid :: ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
bid = do
  (FakeBidTX _ _ coinTXID coinSCID coinVersion) <- placeFakeBid
  placeBid coinTXID coinSCID coinVersion

placeFakeBid :: ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
placeFakeBid = do
  config@(BidConfig key aucTXID coinTXID) <- ask
  TXSummary{..} <- postTX (FakeBidTXin key aucTXID coinTXID)
  FakeBidTXResponse {
    coinSCID=txInputVersion,
    coinVersion="",
    ..
    }

  maybe
        (throwError $ TXBodyFailed stdOut)
        (\FakeBidTXout {..} ->
           return $ FakeBidTX key aucTXID coinTXID coinSCID coinVersion)
        (runReaderT (fakeBidParser stdOut) config)

placeBid ::
     CoinTXID
  -> CoinSCID
  -> CoinVersion
  -> ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
placeBid coinTXID coinSCID coinVersion = do
  config@(BidConfig key aucTXID coinTXID) <- ask
  stdOut <-
    postTX (BidTXin key aucTXID coinTXID coinSCID coinVersion)
  maybe
        (throwError $ TXBodyFailed stdOut)
        (\BidTXout {..} -> return $ BidTX txid aucTXID coinTXID isWinningBid)
        (runReaderT (bidParser stdOut) config)

createAuction :: ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
createAuction = do
  (CreateAuctionConfig key) <- ask
  TXSummary{..} <- postTX (CreateAuctionTXin key)
  return $ AuctionCreatedTX transactionID

getCoin :: ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
getCoin = do
  (GetCoinConfig key) <- ask
  TXSummary{..} <- postTX (GetCoinTXin key)
  return $ GetCoinTX transactionID

-- take the coins from an old cache destroy the cache and deposit the old coins + 1 new coin to a new cache
getMoreCoins :: ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
getMoreCoins = do
  (GetMoreCoinsConfig key coinTXID) <- ask
  stdOut <- postTX (GetMoreCoinsTXin key coinTXID)
  maybe
        (throwError $ TXBodyFailed stdOut)
        (\(GetMoreCoinsTXout txid) -> return $ GetMoreCoinsTX txid)
        (getMoreCoinsParser stdOut)

withdraw :: ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
withdraw = do
  (WithdrawConfig key aucTXID) <- ask
  txOutJSON <- postTX (WithdrawTXin key aucTXID)
  maybe
        (throwError $ TXBodyFailed txOutJSON)
        (\(WithdrawTXout txid) -> return $ WithdrawTX txid)
        (txOutJSON)
