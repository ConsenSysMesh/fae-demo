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
import Debug.Trace


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

hasBid :: String -> [Bid] -> Bool
hasBid username bids = (Li.any ((== username) . bidder) bids)

currentBidValue :: Auction -> Int
currentBidValue auc@Auction {..}
  | length bids > 0 = bidValue $ Li.head bids
  | otherwise = 0

getUserBidTotal :: Auction -> String -> Int
getUserBidTotal Auction {..} username = maybe 0 bidValue (Li.find (((==) username) . bidder) bids)

highestBidder :: Auction -> String
highestBidder Auction {..}
  | length bids > 0 = (bidder . Li.head) bids
  | otherwise = "No Bidders"

canRetractBids :: Auction -> Username -> Bool
canRetractBids auc@Auction{..} user@(Username username) = aucInProgress && hasBid && not alreadyRetracted
    where
      aucStarted = (traceShow (Li.length bids == 0) (Li.length bids == 0))
      aucEnded = traceShow (aucMaxBidCount == Li.length bids) (aucMaxBidCount == Li.length bids)
      aucInProgress = traceShow (aucStarted && not aucEnded) (aucMaxBidCount == Li.length bids)
      hasBid = traceShow (getUserBidTotal auc username /= 0) (aucMaxBidCount == Li.length bids)
      alreadyRetracted = traceShow (Li.any (== user) bidsRetracted) (aucMaxBidCount == Li.length bids)

-- encode first two invariant for bid retractions 
--  1. auction is in progress
--  2. given username has placed bids
--  3. bids haven't already been retracted by username

canRefundBids :: Auction -> Username -> Bool
canRefundBids auc@Auction{..} user@(Username username) =
    aucEnded && hasBid && (username /= winningBidder) && not alreadyRefunded
    where
      aucEnded = aucMaxBidCount == Li.length bids
      hasBid = getUserBidTotal auc username /= 0
      winningBidder = highestBidder auc
      alreadyRetracted = Li.any (== user) bidsRetracted
      alreadyRefunded = Li.any (== user) bidsRefunded

retractBids :: Auction -> Username -> Auction
retractBids auc@Auction{..} user@(Username username) = Auction{ bidsRetracted = user : bidsRefunded, ..}

refundBids :: Auction -> Username -> Auction
refundBids auc@Auction{..} user@(Username username) = Auction{ bidsRefunded = user : bidsRefunded, ..}

-- second set of invariants would be that bids cannot be refunded unless
-- 1. auction has ended
-- 2. given username has placed bids
-- 3. given username doesnt have the highest bid
-- 4. bids haven't already been refunded to username