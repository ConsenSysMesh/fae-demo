{-# LANGUAGE RecordWildCards #-}

{----------------------------------------------
  Api for High level fae auction TXs management
-----------------------------------------------}
module FaeTX.Main
  ( bid
 -- , createAuction
 -- , getCoin
  , PostTXResponse
  , PostTXError
  ) where

--  , getMoreCoins
--  , withdraw
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

aid = AucTXID "b65604d5753896b49e9499bb214c5b8107ec5ce3bd5512534a060f9ba9111cef"

cid =
  CoinTXID "3eee5c0fc994fa766f653b60033040040e157ee7d0af20400dd54deb39a6e138"

key1 = Key "bidder1"

postBid :: Key -> AucTXID -> CoinTXID -> IO (Either PostTXError PostTXResponse)
postBid key aucTXID coinTXID = runReaderT (runExceptT bid) bidConfig
  where
    bidConfig = BidConfig key aucTXID coinTXID

--execute = runExceptT . flip runReaderT
executeContract :: TXConfig -> IO (Either PostTXError PostTXResponse)
executeContract conf@BidConfig {} = runReaderT (runExceptT bid) conf
executeContract conf@CreateAuctionConfig {} =
  runReaderT (runExceptT createAuction) conf
executeContract conf@GetCoinConfig {} = runReaderT (runExceptT getCoin) conf
executeContract conf@GetMoreCoinsConfig {} =
  runReaderT (runExceptT getMoreCoins) conf
executeContract conf@WithdrawConfig {} = runReaderT (runExceptT withdraw) conf

bid :: ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
bid = do
  (BidConfig key aucTXID coinTXID) <- ask
  (FakeBid key aucTXID coinTXID coinSCID coinVersion) <- placeFakeBid
  placeBid coinTXID coinSCID coinVersion

placeFakeBid :: ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
placeFakeBid = do
  config@(BidConfig key aucTXID coinTXID) <- ask
  (exitCode, stdOut, stdErr) <- postTX (FakeBidTXin key aucTXID coinTXID)
  case exitCode of
    ExitSuccess ->
      maybe
        (throwError $ TXBodyFailed stdOut)
        (\FakeBidTXout {..} ->
           return $ FakeBid key aucTXID coinTXID coinSCID coinVersion)
        (runReaderT (fakeBidParser stdOut) config)
    ExitFailure _ -> throwError (TXFailed stdErr)

placeBid ::
     CoinTXID
  -> CoinSCID
  -> CoinVersion
  -> ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
placeBid coinTXID coinSCID coinVersion = do
  config@(BidConfig key aucTXID coinTXID) <- ask
  (exitCode, stdOut, stdErr) <-
    postTX (BidTXin key aucTXID coinTXID coinSCID coinVersion)
  case exitCode of
    ExitSuccess ->
      maybe
        (throwError $ TXBodyFailed stdOut)
        (\BidTXout {..} -> return $ Bid txid aucTXID isWinningBid)
        (runReaderT (bidParser stdOut) config)
    ExitFailure _ -> throwError $ TXFailed stdErr

createAuction :: ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
createAuction = do
  config@(CreateAuctionConfig key) <- ask
  (exitCode, stdOut, stdErr) <- postTX (CreateAuctionTXin key)
  case exitCode of
    ExitSuccess ->
      maybe
        (throwError $ TXBodyFailed stdOut)
        (\(CreateAuctionTXout txid) -> return $ CreateAuction txid)
        (createAuctionParser stdOut)
    ExitFailure _ -> throwError $ TXFailed stdErr

getCoin :: ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
getCoin = do
  config@(GetCoinConfig key) <- ask
  (exitCode, stdOut, stdErr) <- postTX (GetCoinTXin key)
  case exitCode of
    ExitSuccess ->
      maybe
        (throwError $ TXBodyFailed stdOut)
        (\(GetCoinTXout txid) -> return $ GetCoin txid)
        (getCoinParser stdOut)
    ExitFailure _ -> throwError $ TXFailed stdErr

-- take the coins from an old cache destroy the cache and deposit the old coins + 1 new coin to a new cache
getMoreCoins :: ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
getMoreCoins = do
  config@(GetMoreCoinsConfig key coinTXID) <- ask
  (exitCode, stdOut, stdErr) <- postTX (GetMoreCoinsTXin key coinTXID)
  case exitCode of
    ExitSuccess ->
      maybe
        (throwError $ TXBodyFailed stdOut)
        (\(GetMoreCoinsTXout txid) -> return $ GetMoreCoins txid)
        (getMoreCoinsParser stdOut)
    ExitFailure _ -> throwError $ TXFailed stdErr

withdraw :: ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
withdraw = do
  config@(WithdrawConfig key aucTXID) <- ask
  (exitCode, stdOut, stdErr) <- postTX (WithdrawTXin key aucTXID)
  case exitCode of
    ExitSuccess ->
      maybe
        (throwError $ TXBodyFailed stdOut)
        (\(WithdrawTXout txid) -> return $ Withdraw txid)
        (withdrawParser stdOut)
    ExitFailure _ -> throwError $ TXFailed stdErr
