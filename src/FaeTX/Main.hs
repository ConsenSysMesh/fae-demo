{-# LANGUAGE RecordWildCards #-}

{----------------------------------------------
  Api for High level fae auction TXs management
-----------------------------------------------}
module FaeTX.Main
  --( bid
  --, createAuction
  --, getCoin
  --, getMoreCoins
  --, withdraw
  --, PostTXResponse
  --, PostTXError
 -- )
 where

import Control.Error.Util
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import FaeTX.Incoming.ParseTX
import FaeTX.Incoming.Types
import FaeTX.Outgoing.PostTX
import FaeTX.Outgoing.Types
import Prelude
import System.Exit

import FaeTX.Types

type TX = ExceptT PostTXError (ReaderT BidConfig IO) PostTXResponse

aid = AucTXID "b65604d5753896b49e9499bb214c5b8107ec5ce3bd5512534a060f9ba9111cef"
cid = CoinTXID "3eee5c0fc994fa766f653b60033040040e157ee7d0af20400dd54deb39a6e138"
key1 = Key "bidder1"

runBid :: Key -> AucTXID -> CoinTXID -> IO (Either PostTXError PostTXResponse)
runBid key aucTXID coinTXID = runReaderT (runExceptT bid) bidConfig 
   where bidConfig = BidConfig {..}

bid :: ExceptT PostTXError (ReaderT BidConfig IO) PostTXResponse
bid = do
  bidConf@BidConfig {..} <- ask
  (FakeBid key aucTXID coinTXID coinSCID coinVersion) <- placeFakeBid
  placeBid coinTXID coinSCID coinVersion

placeFakeBid :: ExceptT PostTXError (ReaderT BidConfig IO) PostTXResponse
placeFakeBid = do
  bidConf@BidConfig {..} <- ask
  (exitCode, stdOut, stdErr) <- postTX (FakeBidTXin key aucTXID coinTXID) -- define record type instead of using an tuple
  case exitCode of
    ExitSuccess ->
      maybe
        (throwError $ TXBodyFailed stdOut)
        (\FakeBidTXout {..} ->
           return $ FakeBid key aucTXID coinTXID coinSCID coinVersion)
        (runReaderT (fakeBidParser stdOut) bidConf)
    ExitFailure _ -> throwError (TXFailed stdErr)

placeBid ::
     CoinTXID
  -> CoinSCID
  -> CoinVersion
  -> ExceptT PostTXError (ReaderT BidConfig IO) PostTXResponse
placeBid coinTXID coinSCID coinVersion = do
  bidConf@BidConfig {..} <- ask
  (exitCode, stdOut, stdErr) <-
    postTX (BidTXin key aucTXID coinTXID coinSCID coinVersion)
  case exitCode of
    ExitSuccess ->
      maybe
        (throwError $ TXBodyFailed stdOut)
        (\BidTXout {..} -> return $ Bid txid aucTXID isWinningBid)
        (runReaderT (bidParser stdOut) bidConf)
    ExitFailure _ -> throwError $ TXFailed stdErr
{-
createAuction :: Key -> IO (Either PostTXError PostTXResponse)
createAuction key =
  postTX (CreateAuctionTXin key) >>= \(exitCode, stdOut, stdErr) ->
    return $
    case exitCode of
      ExitSuccess ->
        maybe
          (Left $ TXBodyFailed stdOut)
          (\(CreateAuctionTXout txid) -> Right $ CreateAuction txid)
          (createAuctionParser stdOut)
      ExitFailure _ -> Left $ TXFailed stdErr

getCoin :: Key -> IO (Either PostTXError PostTXResponse)
getCoin key =
  postTX (GetCoinTXin key) >>= \(exitCode, stdOut, stdErr) ->
    return $
    case exitCode of
      ExitSuccess ->
        maybe
          (Left $ TXBodyFailed stdOut)
          (\(GetCoinTXout txid) -> Right $ GetCoin txid)
          (getCoinParser stdOut)
      ExitFailure _ -> Left $ TXFailed stdErr

-- take the coins from an old cache destroy the cache and deposit the old coins + 1 new coin to a new cache
getMoreCoins :: Key -> CoinTXID -> IO (Either PostTXError PostTXResponse)
getMoreCoins key coinTXID =
  postTX (GetMoreCoinsTXin key coinTXID) >>= \(exitCode, stdOut, stdErr) ->
    return $
    case exitCode of
      ExitSuccess ->
        maybe
          (Left $ TXBodyFailed stdOut)
          (\(GetMoreCoinsTXout txid) -> Right $ GetMoreCoins txid)
          (getMoreCoinsParser stdOut)
      ExitFailure _ -> Left $ TXFailed stdErr

withdraw :: Key -> AucTXID -> IO (Either PostTXError PostTXResponse)
withdraw key aucTXID =
  postTX (WithdrawTXin key aucTXID) >>= \(exitCode, stdOut, stdErr) ->
    return $
    case exitCode of
      ExitSuccess ->
        maybe
          (Left $ TXBodyFailed stdOut)
          (\(WithdrawTXout txid) -> Right $ Withdraw txid)
          (withdrawParser stdOut)
      ExitFailure _ -> Left $ TXFailed stdErr
-}
