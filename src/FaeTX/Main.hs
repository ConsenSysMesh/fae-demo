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
  , PostTXResponse
  , PostTXError
  ) where

import Control.Monad
import Data.List
import Data.Monoid
import Debug.Trace
import FaeTX.Incoming.ParseTX
import FaeTX.Incoming.Types
import FaeTX.Outgoing.FormatTX
import FaeTX.Outgoing.PostTX
import FaeTX.Outgoing.Types
import Prelude
import System.Process
import Text.Pretty.Simple (pPrint)

import FaeTX.Types

getBidTXin :: AuctionTXout -> AuctionTXin
getBidTXin (FakeBidTXout key _ aucTXID coinTXID coinSCID coinVersion) =
  (BidTXin (key) (aucTXID) (coinTXID) (coinSCID) (coinVersion))

placeBid ::
     Key
  -> AucTXID
  -> CoinTXID
  -> CoinSCID
  -> CoinVersion
  -> IO (Either PostTXError PostTXResponse)
placeBid key aucTXID coinTXID coinSCID coinVersion =
  postTX (BidTXin key aucTXID coinTXID coinSCID coinVersion) >>= \postTXStdOut ->
    pure $
    maybe
      (Left $ TXBodyFailed postTXStdOut)
      (\(BidTXout txID aucTXID _ _ _ isWinningBid) ->
         Right $ Bid txID aucTXID isWinningBid)
      (bidParser aucTXID coinTXID postTXStdOut)

--main = 
main = getMoreCoins key coinid >>= print
  --bid key auctionid coinid >>= print 
  --getMoreCoins key coinid
--getCoin key >>= print
key = "bidder1"

auctionid = "e673db9705c9c85e65e0fe6e9f9b2eb13195c244daf4cb36e8aa48c223780804"

coinid = "f926cd2fd6c82a6157d5f6f887fec77e596b08b4406300163ce63a4710dbd9c6"
 -- 6041899bdfccf2ec1008a1c9f7fe323254c6fbf067fd794f210742712ccda2b0
 -- edc29dfeb8fe80ac348b59658b67a53cad2a0b25dd0951f572b170e0c4e36bc1

--callFaeContract txInput postTXstdoutParser = postTx txInput >>= pure . postTXstdoutParser
placeFakeBid ::
     Key -> AucTXID -> CoinTXID -> IO (Either PostTXError PostTXResponse)
placeFakeBid key aucTXID coinTXID =
  postTX fakeBid >>= \postTXstdout ->
    pure $
    maybe
      (Left $ TXBodyFailed postTXstdout)
      (\(FakeBidTXout key _ aucTXID coinTXID coinSCID coinVersion) ->
         Right (FakeBid key aucTXID coinTXID coinSCID coinVersion))
      (fakeBidParser key aucTXID coinTXID postTXstdout)
  where
    fakeBid = FakeBidTXin key aucTXID coinTXID

bid :: String -> String -> String -> IO (Either PostTXError PostTXResponse)
bid key aucTXID coinTXID =
  placeFakeBid (Key key) (AucTXID aucTXID) (CoinTXID coinTXID) >>=
  (\fakeBidOutput ->
     (either
        (pure . Left . id)
        (\(FakeBid key aucTXID coinTXID coinSCID coinVersion) ->
           placeBid key aucTXID coinTXID coinSCID coinVersion)
        fakeBidOutput))

createAuction :: String -> IO (Either PostTXError PostTXResponse)
createAuction key =
  postTX (CreateAuctionTXin (Key key)) >>= \postTXStdOut ->
    pure $
    maybe
      (Left $ TXBodyFailed postTXStdOut)
      (\(CreateAuctionTXout txid) -> Right $ CreateAuction txid)
      (createAuctionParser postTXStdOut)

getCoin :: String -> IO (Either PostTXError PostTXResponse)
getCoin key =
  postTX (GetCoinTXin (Key key)) >>= \postTXStdOut ->
    pure $
    maybe
      (Left $ TXBodyFailed postTXStdOut)
      (\(GetCoinTXout txid) -> Right $ GetCoin txid)
      (getCoinParser postTXStdOut)

-- take the coins from an old cache destroy the cache and deposit the old coins + 1 new coin to a new cache
getMoreCoins :: String -> String -> IO (Either PostTXError PostTXResponse)
getMoreCoins key coinTXID =
  postTX (GetMoreCoinsTXin (Key key) (CoinTXID coinTXID)) >>= \postTXStdOut ->
    pure $
    maybe
      (Left $ TXBodyFailed postTXStdOut)
      (\(GetMoreCoinsTXout txid) -> Right $ GetMoreCoins txid)
      (getMoreCoinsParser postTXStdOut)
--withdraw :: String -> String -> IO (Either PostTXError PostTXResponse)
--withdraw key aucTXID=
--  postTX (WithdrawCoinTXin (Key key) (AucTXID aucTXID)) >>=
--  pure . withdrawParser
