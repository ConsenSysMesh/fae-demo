{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

----------------------------------------------
-- Post Auction Transactions To Fae
----------------------------------------------
module AuctionManager where

import qualified Data.List as Li
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Monoid
import PostTX
import Prelude
import Types
import SharedTypes 
import PostTX.Types

updateAuctionState :: ServerState -> Map AucTXID Auction -> ServerState
updateAuctionState ServerState {..} auctionState =
  ServerState {auctions = auctionState, ..}

updateAuctionWithBid ::
     AucTXID -> Bid -> Map AucTXID Auction -> Map AucTXID Auction
updateAuctionWithBid aucTXID bid =
  Map.adjust (\Auction {..} -> Auction {bids = bid : bids, ..}) aucTXID

createAuction ::
     AucTXID -> Auction -> Map AucTXID Auction -> Map AucTXID Auction
createAuction = Map.insert

postCreateAuctionTX :: Key -> Int -> Int -> IO (Either PostTXError PostTXResponse)
postCreateAuctionTX key startingVal aucMaxBidCount = executeContract (CreateAuctionConfig key (AucStartingValue startingVal) (MaxBidCount aucMaxBidCount))

postBidTX ::
     Key -> AucTXID -> CoinTXID -> IO (Either PostTXError PostTXResponse)
postBidTX key aucTXID coinTXID = executeContract (BidConfig key aucTXID coinTXID)

auctionStatus :: Auction -> String
auctionStatus auc@Auction {..}
  | numBids auc < aucMaxBidCount = highBidder <> "is Winning"
  | numBids auc == 0 = "No Bids yet"
  | otherwise = highBidder <> " Has Won!"
  where
    highBidder = highestBidder auc

getBidValue :: Bid -> Int
getBidValue Bid {..} = bidValue

numBids :: Auction -> Int
numBids Auction {..} = length bids

getBidder :: Bid -> String
getBidder Bid {..} = bidder

currentBidValue :: Auction -> Int
currentBidValue Auction {..}
  | length bids > 0 = (getBidValue . last) bids
  | otherwise = startingValue

highestBidder :: Auction -> String
highestBidder Auction {..}
  | length bids > 0 = (getBidder . Li.last) bids
  | otherwise = "No Bidders"
