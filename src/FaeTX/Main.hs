{-# LANGUAGE OverloadedStrings #-}
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

{-
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
 -- , coinTXID :: CoinTXID
 -}
type TX = ExceptT PostTXError (ReaderT BidConfig IO) PostTXResponse --(exitCode, stdOut, stdErr) <- postTx =<< uncurry3 FakeBidTXin <$> ask

bid :: BidConfig -> ReaderT BidConfig IO (Either PostTXError PostTXResponse)
bid conf = do
  postTXResult <- runExceptT placeFakeBid
  return postTXResult

--flip runReaderT (key, auc, coin)                                 -- access bidconfig
getKey :: Key -> ReaderT Key IO ()
getKey key = ReaderT $ \key -> return ()

placeFakeBid :: ExceptT PostTXError (ReaderT BidConfig IO) PostTXResponse
placeFakeBid = do
  bidConf@BidConfig {..} <- ask
  (exitCode, stdOut, stdErr) <- postTX (FakeBidTXin key aucTXID coinTXID) -- define record type instead of using an tuple
  case exitCode of
    ExitSuccess ->
      maybe -- use fmap  and `note` instead of maybe
        (throwError $ TXBodyFailed stdOut)
        (\(FakeBidTXout _ _ _ _ coinSCID coinVersion) ->
           return (FakeBid key aucTXID coinTXID coinSCID coinVersion))
        (runReaderT (fakeBidParser stdOut) bidConf)
    ExitFailure _ -> throwError (TXFailed stdErr)
{-
--main = undefined
--  [13:09] <lyxia> so every time you have  (f key aucTXID coinTXID)  you could replace that with  f  and change the type of f from
--     (f :: Key -> AucTXID -> CoinTXID -> x ghci
--    -> IO y)   to   (f :: x -> ReaderT Env IO y)   with   (type Env = (Key, AncTXID, CoinTXID))
-}
   {-
bid :: Key -> AucTXID -> CoinTXID -> IO (Either PostTXError PostTXResponse)
bid key aucTXID coinTXID =
  placeFakeBid key aucTXID coinTXID >>=
  (\fakeBidOutput ->
     (either
        (pure . Left)
        (\(FakeBid _ _ _ coinSCID coinVersion) ->
           placeBid key aucTXID coinTXID coinSCID coinVersion)
        fakeBidOutput))

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

-- take the coins from an old cache destroy the cache staand deposit the old coins + 1 new coin to a new cache
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
