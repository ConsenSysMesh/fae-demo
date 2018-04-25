{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{----------------------------------------------
  Api for High level fae auction TXs management
-----------------------------------------------}
module FaeTX.Main where

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

callContract :: AuctionTXin -> IO String
callContract (GetCoinTXin key) = postTX (GetCoinTXin key)
callContract (CreateAuctionTXin key) = postTX (CreateAuctionTXin key)
callContract (WithdrawCoinTXin key aucTXID) =
  postTX (WithdrawCoinTXin key aucTXID)
callContract (FakeBidTXin key aucTXID coinTXID) =
  postTX (FakeBidTXin key aucTXID coinTXID)
callContract (BidTXin (key) (aucTXID) (coinTXID) (coinSCID) (coinVersion)) =
  postTX (BidTXin (key) (aucTXID) (coinTXID) (coinSCID) (coinVersion))

placeFakeBid :: Key -> AucTXID -> CoinTXID -> IO (Maybe AuctionTXout)
placeFakeBid key aucTXID coinTXID =
  postTX fakeBid >>= pure . fakeBidParser key aucTXID coinTXID
  where
    fakeBid = FakeBidTXin key aucTXID coinTXID

getBidTXin :: AuctionTXout -> AuctionTXin
getBidTXin (FakeBidTXout key _ aucTXID coinTXID coinSCID coinVersion) =
  (BidTXin (key) (aucTXID) (coinTXID) (coinSCID) (coinVersion))

placeBid :: AuctionTXout -> IO (Maybe AuctionTXout)
placeBid fBid@(FakeBidTXout key _ aucTXID coinTXID _ _) =
  (postTX $ getBidTXin fBid) >>= pure . bidParser key aucTXID coinTXID

k = Key "bidder1"

aucid =
  AucTXID "ea4e069cdd8e2367415d735d083b9dc31c94f1e30f7fcc3f667dd13144e419c1"

cointxid =
  CoinTXID "864407287131a97c3ee07cb6aab4c6a6cae9ae6a80e381055393256b1adb387b"
