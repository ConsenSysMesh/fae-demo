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
import qualified Data.List as Li
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Monoid 

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D
import Prelude
import Types
import SharedTypes

validBid :: Bid -> Auction -> Bool
validBid Bid {..} a@Auction {..} = undefined
  where
    numBids = length bids

bidOnAuction :: AucTXID -> Bid -> Map AucTXID Auction -> Map AucTXID Auction
bidOnAuction key (bid@Bid {..}) =
  Map.adjust
    (\auction@Auction {..} -> Auction {bids = bid : bids, ..})
    key

createAuction ::
     AucTXID -> Auction -> Map AucTXID Auction -> Map AucTXID Auction
createAuction aucTXID auction auctionsMap =
  Map.insert aucTXID auction auctionsMap

auctionStatus :: Auction -> String
auctionStatus auc@Auction {..}
  | noBids == 0             = "No Bids"
  | getIsWinningBid lastBid = getBidder lastBid <> " has Won"
  | otherwise               = highBidder <> " is Winning"
  where
    noBids = numBids auc
    lastBid = head bids
    highBidder = highestBidder auc
    getBidder Bid{..} = bidder

auctionEnded :: Auction -> Bool
auctionEnded Auction{..} = length bids == aucMaxBidCount

getIsWinningBid :: Bid -> Bool
getIsWinningBid Bid{..} = isWinningBid

getBidValue :: Bid -> Int
getBidValue Bid {..} = bidValue

numBids :: Auction -> Int
numBids Auction {..} = Prelude.length bids

getBidder :: Bid -> String
getBidder Bid {..} = bidder

currentBidValue :: Auction -> Int
currentBidValue Auction {..}
  | length bids > 0 = (getBidValue . head) bids
  | otherwise = startingValue

highestBidder :: Auction -> String
highestBidder Auction {..}
  | length bids > 0 = (getBidder . Li.head) bids
  | otherwise = "No Bidders"
