{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

----------------------------------------------
-- Post Auction Transactions To Fae
----------------------------------------------
module Auction where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import qualified Data.List as Li
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Monoid
import FaeTX.Types (PostTXResponse)
import Prelude
import Types

bid = undefined

createAuction = undefined

updateAuctionState :: ServerState -> Map String Auction -> ServerState
updateAuctionState ServerState {..} auctionState =
  ServerState {auctions = auctionState, ..}

isValidAuctionAction :: Msg -> Map String Auction -> Bool
isValidAuctionAction (BidAuctionAction aucId bid) auctions =
  case Map.lookup aucId auctions of
    (Just auction) -> validBid bid auction
    Nothing -> False
isValidAuctionAction (CreateAuctionAction Auction {..}) auctions =
  not $ Map.member auctionId auctions

validBid :: Bid -> Auction -> Bool
validBid Bid {..} a@Auction {..} =
  bidValue > (currentBidValue a) && numBids < maxNumBids
  where
    numBids = length bids

bidOnAuction :: String -> Bid -> Map String Auction -> Map String Auction
bidOnAuction aucId (bid@Bid {..}) =
  Map.adjust
    (\auction@Auction {..} ->
       case validBid bid auction of
         True -> Auction {bids = bid : bids, ..}
         False -> auction)
    key
  where
    key = aucId

createAuction :: PostTXResponse -> Map String Auction -> Map String Auction
createAuction auction auctionsMap = undefined

auctionStatus :: Auction -> String
auctionStatus auc@Auction {..}
  | numBids auc < maxNumBids = highBidder <> "is Winning"
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
