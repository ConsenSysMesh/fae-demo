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

placeFakeBid :: Key -> AucTXID -> CoinTXID -> IO (Maybe AuctionTXout)
placeFakeBid key aucTXID coinTXID =
  postTX fakeBid >>= pure . (fakeBidParser key aucTXID coinTXID)
  where
    fakeBid = FakeBidTXin key aucTXID coinTXID

getBidTXin :: AuctionTXout -> AuctionTXin
getBidTXin (FakeBidTXout key _ aucTXID coinTXID coinSCID coinVersion) =
  (BidTXin (key) (aucTXID) (coinTXID) (coinSCID) (coinVersion))

placeBid :: AuctionTXout -> IO (Maybe AuctionTXout)
placeBid fBid@(FakeBidTXout key _ aucTXID coinTXID _ _) =
  (postTX $ getBidTXin fBid) >>= pure . bidParser key aucTXID coinTXID

bid key auctionid coinid =
  (placeFakeBid key auctionid coinid) >>= \fBid ->
    case fBid of
      Just fbidOut -> placeBid fbidOut
      Nothing -> pure Nothing

    
createAuction key =  (postTX (CreateAuctionTXin key)) >>= pure .  createAuctionParser >>= print

getCoin key = postTX (GetCoinTXin key) >>= pure . getCoinParser

--main = 
main = bid key auctionid coinid >>= print
--getCoin key >>= print
key = Key "bidder1"

auctionid =
  AucTXID "e673db9705c9c85e65e0fe6e9f9b2eb13195c244daf4cb36e8aa48c223780804"

coinid =
  CoinTXID "30ba7220eef7ac67ca61d72b1ce82501714990391cd1e02a66f78a41782dfe74"
