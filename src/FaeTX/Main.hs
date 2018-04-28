{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{----------------------------------------------
  Api for High level fae auction TXs management
-----------------------------------------------}
module FaeTX.Main
  ( bid
  , createAuction
  , getCoin
  , getMoreCoins
  , withdraw
  , PostTXResponse
  , PostTXError
  ) where

import Control.Monad
import FaeTX.Incoming.ParseTX
import FaeTX.Incoming.Types
import FaeTX.Outgoing.PostTX
import FaeTX.Outgoing.Types
import Prelude
import System.Exit

import FaeTX.Types

placeBid ::
     Key
  -> AucTXID
  -> CoinTXID
  -> CoinSCID
  -> CoinVersion
  -> IO (Either PostTXError PostTXResponse)
placeBid key aucTXID coinTXID coinSCID coinVersion =
  postTX (BidTXin key aucTXID coinTXID coinSCID coinVersion) >>= \(exitCode, stdOut, stdErr) ->
    return $
    case exitCode of
      ExitSuccess ->
        maybe
          (Left $ TXBodyFailed stdOut)
          (\(BidTXout txID _ _ _ _ isWinningBid) ->
             Right $ Bid txID aucTXID isWinningBid)
          (bidParser aucTXID coinTXID stdOut)
      ExitFailure _ -> Left $ TXFailed stdErr

placeFakeBid ::
     Key -> AucTXID -> CoinTXID -> IO (Either PostTXError PostTXResponse)
placeFakeBid key aucTXID coinTXID =
  postTX fakeBid >>= \(exitCode, stdOut, stdErr) ->
    return $
    case exitCode of
      ExitSuccess ->
        maybe
          (Left $ TXBodyFailed stdOut)
          (\(FakeBidTXout _ _ _ _ coinSCID coinVersion) ->
             Right (FakeBid key aucTXID coinTXID coinSCID coinVersion))
          (fakeBidParser key aucTXID coinTXID stdOut)
      ExitFailure _ -> Left $ TXFailed stdErr
  where
    fakeBid = FakeBidTXin key aucTXID coinTXID

bid :: String -> String -> String -> IO (Either PostTXError PostTXResponse)
bid key aucTXID coinTXID =
  placeFakeBid (Key key) (AucTXID aucTXID) (CoinTXID coinTXID) >>=
  (\fakeBidOutput ->
     (either
        (pure . Left)
        (\(FakeBid key aucTXID coinTXID coinSCID coinVersion) ->
           placeBid key aucTXID coinTXID coinSCID coinVersion)
        fakeBidOutput))

createAuction :: String -> IO (Either PostTXError PostTXResponse)
createAuction key =
  postTX (CreateAuctionTXin (Key key)) >>= \(exitCode, stdOut, stdErr) ->
    return $
    case exitCode of
      ExitSuccess ->
        maybe
          (Left $ TXBodyFailed stdOut)
          (\(CreateAuctionTXout txid) -> Right $ CreateAuction txid)
          (createAuctionParser stdOut)
      ExitFailure _ -> Left $ TXFailed stdErr

getCoin :: String -> IO (Either PostTXError PostTXResponse)
getCoin key =
  postTX (GetCoinTXin (Key key)) >>= \(exitCode, stdOut, stdErr) ->
    return $
    case exitCode of
      ExitSuccess ->
        maybe
          (Left $ TXBodyFailed stdOut)
          (\(GetCoinTXout txid) -> Right $ GetCoin txid)
          (getCoinParser stdOut)
      ExitFailure _ -> Left $ TXFailed stdErr

-- take the coins from an old cache destroy the cache staand deposit the old coins + 1 new coin to a new cache
getMoreCoins :: String -> String -> IO (Either PostTXError PostTXResponse)
getMoreCoins key coinTXID =
  postTX (GetMoreCoinsTXin (Key key) (CoinTXID coinTXID)) >>= \(exitCode, stdOut, stdErr) ->
    return $
    case exitCode of
      ExitSuccess ->
        maybe
          (Left $ TXBodyFailed stdOut)
          (\(GetMoreCoinsTXout txid) -> Right $ GetMoreCoins txid)
          (getMoreCoinsParser stdOut)
      ExitFailure _ -> Left $ TXFailed stdErr

withdraw :: String -> String -> IO (Either PostTXError PostTXResponse)
withdraw key aucTXID =
  postTX (WithdrawTXin (Key key) (AucTXID aucTXID)) >>= \(exitCode, stdOut, stdErr) ->
    return $
    case exitCode of
      ExitSuccess ->
        maybe
          (Left $ TXBodyFailed stdOut)
          (\(WithdrawTXout txid) -> Right $ Withdraw txid)
          (withdrawParser stdOut)
      ExitFailure _ -> Left $ TXFailed stdErr
