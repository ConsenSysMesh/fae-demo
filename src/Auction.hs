{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

----------------------------------------------
-- Post Auction Transactions To Fae
----------------------------------------------
module Auction where

import qualified Data.List as Li
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Monoid
import FaeTX.Post
import Prelude
import Types

updateAuctionState :: ServerState -> Map AucTXID Auction -> ServerState
updateAuctionState ServerState {..} auctionState =
  ServerState {auctions = auctionState, ..}

updateAuctionWithBid ::
     AucTXID -> Bid -> Map AucTXID Auction -> Map AucTXID Auction
updateAuctionWithBid aucTXID bid =
  Map.adjust (\Auction {..} -> Auction {bids = bid : bids, ..}) aucTXID

postCreateAuctionTX :: Key -> IO (Either PostTXError PostTXResponse)
postCreateAuctionTX key = executeContract (CreateAuctionConfig key)

postBidTX ::
     Key -> AucTXID -> CoinTXID -> IO (Either PostTXError PostTXResponse)
postBidTX key aucTXID coinTXID =
  executeContract (BidConfig key aucTXID coinTXID)

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
