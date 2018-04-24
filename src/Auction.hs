{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{--------------------------------------------------------
  Logic for Updating Auction State
---------------------------------------------------------}
module Auction where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Data.Aeson
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.List as Li
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D
import qualified Network.WebSockets as WS
import Prelude
import Types

validBid :: Bid -> Auction -> Bool
validBid Bid {..} a@Auction {..} =
  bidValue > (currentBidValue a) && numBids < maxNumBids
  where
    numBids = length bids
    -- the output of PostTX should decide this

bidOnAuction :: AuctionId -> Bid -> IntMap Auction -> IntMap Auction
bidOnAuction key (bid@Bid {..}) =
  IntMap.adjust
    (\auction@Auction {..} ->
       case validBid bid auction of
         True -> Auction {bids = bid : bids, ..}
         False -> auction)
    key

createAuction :: Auction -> IntMap Auction -> IntMap Auction
createAuction auction auctionsMap = IntMap.insert key auction auctionsMap
  where
    key = getNextAuctionKey auctionsMap

getNextAuctionKey :: IntMap Auction -> IntMap.Key
getNextAuctionKey a =
  case IntMap.maxViewWithKey a of
    (Just ((k, _), _)) -> k + 1
    Nothing -> 1

auctionStatus :: Auction -> String
auctionStatus auc@Auction {..}
  | (numBids auc) < maxNumBids = highBidder <> "is Winning"
  | numBids auc == 0 = "No Bids yet"
  | otherwise = highBidder <> " Has Won!"
  where
    highBidder = highestBidder auc

getBidValue :: Bid -> Int
getBidValue Bid {..} = bidValue

numBids :: Auction -> Int
numBids Auction {..} = Prelude.length bids

getBidder :: Bid -> String
getBidder Bid {..} = bidder

currentBidValue :: Auction -> Int
currentBidValue Auction {..}
  | length bids > 0 = (getBidValue . Li.last) bids
  | otherwise = initialValue

highestBidder :: Auction -> String
highestBidder Auction {..}
  | length bids > 0 = (getBidder . Li.last) bids
  | otherwise = "No Bidders"
