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
import System.Exit

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
  postTX (BidTXin key aucTXID coinTXID coinSCID coinVersion) >>= \(exitCode, stdOut, stdErr) ->
  return $ case exitCode of
      ExitSuccess ->
        maybe
          (Left $ TXBodyFailed stdOut)
          (\(BidTXout txID _ _ _ _ isWinningBid) ->
            Right $ Bid txID aucTXID isWinningBid)
          (bidParser aucTXID coinTXID stdOut)
      ExitFailure _ -> Left $ TXFailed stdErr

--main = 
main = bid key auctionid coinid >>= print
  --bid key auctionid coinid >>= print 
  --getMoreCoins key coinid
--getCoin key >>= print
key = "bidder1"

auctionid = "e31bdb1d4e539d917dcfd8620e82aa5d777222f3662c8c3dc9482788cec75b4e"
--132cbf82c795d8cc2da3700539b1e5fceca5602847a62daa0ecb390e9679840b
coinid = "0533f1b8b921f343d718bcf2047f962511164a7c7c1fe84b46b3c53b47690c45" -- 2

placeFakeBid ::
     Key -> AucTXID -> CoinTXID -> IO (Either PostTXError PostTXResponse)
placeFakeBid key aucTXID coinTXID =
  postTX fakeBid >>= \(exitCode, stdOut, stdErr) ->
    return $ case exitCode of 
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
   return $ case exitCode of 
      ExitSuccess -> 
          maybe
            (Left $ TXBodyFailed stdOut)
            (\(CreateAuctionTXout txid) -> Right $ CreateAuction txid)
            (createAuctionParser stdOut)
      ExitFailure _ -> Left $ TXFailed stdErr
   

getCoin :: String -> IO (Either PostTXError PostTXResponse)
getCoin key =
  postTX (GetCoinTXin (Key key)) >>= \(exitCode, stdOut, stdErr) ->
    return $ case exitCode of 
      ExitSuccess -> 
        maybe
        (Left $ TXBodyFailed stdOut)
        (\(GetCoinTXout txid) -> Right $ GetCoin txid)
        (getCoinParser stdOut)
      ExitFailure _ -> Left $ TXFailed stdErr

-- take the coins from an old cache destroy the cache and deposit the old coins + 1 new coin to a new cache
getMoreCoins :: String -> String -> IO (Either PostTXError PostTXResponse)
getMoreCoins key coinTXID =
  postTX (GetMoreCoinsTXin (Key key) (CoinTXID coinTXID)) >>= \(exitCode, stdOut, stdErr) ->
    return $ case exitCode of 
      ExitSuccess -> 
        maybe
        (Left $ TXBodyFailed stdOut)
        (\(GetMoreCoinsTXout txid) -> Right $ GetMoreCoins txid)
        (getMoreCoinsParser stdOut)
      ExitFailure _ -> Left $ TXFailed stdErr

--withdraw :: String -> String -> IO (Either PostTXError PostTXResponse)
--withdraw key aucTXID=
--  postTX (WithdrawCoinTXin (Key key) (AucTXID aucTXID)) >>=
--  pure . withdrawParser
