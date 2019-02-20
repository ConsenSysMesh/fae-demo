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

placeBid :: ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
placeBid = do
  config@(BidConfig key aucTXID coinTXID) <- ask
  TXSummary{..} <- postTX (BidTXin key aucTXID coinTXID)
  return $ BidTX transactionID aucTXID coinTXID (TXResult txResult)

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

collect :: ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
collect = do
  (CollectConfig key aucTXID) <- ask
  TXSummary{..} <- postTX (CollectTXin key aucTXID)
  return $ CollectTX transactionID

parseTXSummary :: Text -> Either String TXSummary
parseTXSummary jsonTxt = eitherDecode $ C.pack $ T.unpack jsonTxt

postTX :: AuctionTXin -> ExceptT PostTXError (ReaderT TXConfig IO) (TXSummary)
postTX tx = do
  liftIO $ print command
  (_pIn, pOut, pErr, handle) <- liftIO $ runInteractiveCommand command
  -- Wait for the process to finish and store its exit code
  exitCode <- liftIO $ waitForProcess handle
  -- Get the standard output.
  stdOutput <- liftIO $ hGetContents pOut
  -- return both the output and the exit code.
  liftIO $ print opts
  liftIO $ putStrLn stdOutput
  either (throwError . TXSummaryParseFailed) return (parseTXSummary $ T.pack stdOutput)
  where
    opts@PostTXOpts {..} = traceShow (getPostTXopts tx) (getPostTXopts tx)
    envString = concat $ intersperse " " ((\(a, b) -> concat [ a, "=", b ]) <$> env)
    command = concat $ intersperse " "
        [
          envString,
          "stack",
          "exec",
          "postTX",
          contractName,
          "--",
          "--json"
        ]
