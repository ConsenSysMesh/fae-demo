{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

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
import Data.Maybe 

import Debug.Trace
import PostTX.Types

updateAuctionState :: ServerState -> Map AucTXID Auction -> ServerState
updateAuctionState ServerState {..} auctionState =
  ServerState {auctions = auctionState, ..}

updateAuctionWithBid :: AucTXID -> Bid -> Map AucTXID Auction -> Map AucTXID Auction
updateAuctionWithBid aucTXID bid =
  Map.adjust (\Auction {..} -> Auction {bids = bid : bids, ..}) aucTXID

createAuction :: AucTXID -> Auction -> Map AucTXID Auction -> Map AucTXID Auction
createAuction = Map.insert

postCreateAuctionTX :: Key -> Int -> Int -> IO (Either PostTXError PostTXResponse)
postCreateAuctionTX key startingVal aucMaxBidCount = executeContract (CreateAuctionConfig key (AucStartingValue startingVal) (MaxBidCount aucMaxBidCount))

postBidTX :: Key -> AucTXID -> CoinTXID -> IO (Either PostTXError PostTXResponse)
postBidTX key aucTXID coinTXID = executeContract (BidConfig key aucTXID coinTXID)

postCollectTX :: Key -> AucTXID -> IO (Either PostTXError PostTXResponse)
postCollectTX key aucTXID = executeContract (CollectConfig key aucTXID)

auctionStatus :: Auction -> String
auctionStatus auc@Auction {..}
  | numBids auc < aucMaxBidCount = highBidder <> "is Winning"
  | numBids auc == 0 = "No Bids yet"
  | otherwise = highBidder <> " Has Won!"
  where highBidder = highestBidder auc

getBidValue :: Bid -> Int
getBidValue Bid {..} = bidValue

numBids :: Auction -> Int
numBids Auction {..} = length bids

currentBidValue :: Auction -> Int
currentBidValue Auction {..}
  | length bids > 0 = bidValue $ head bids
  | otherwise = 0
  
getUserBidTotal :: Auction -> String -> Int
getUserBidTotal Auction {..} username = maybe 0 bidValue (Li.find (((==) username) . bidder) bids)

-- use maybe instead
highestBidder :: Auction -> String
highestBidder Auction {..}
  | length bids > 0 = bidder $ Li.head bids
  | otherwise = "No Bidders"

-- if the auction has ended then the collection by the loser doesn't affect the auction
-- otherwise if the auction is in progress then any bidder who isn't winning can retract their
-- bids through calling collect
collect :: Username -> Auction -> (Auction, CoinCollection)
collect username@(Username user) auc@Auction{..} 
    | aucEnded = (,) Auction { bidsRefunded = username : bidsRefunded, .. } $ LoserRefunded userBidsVal
    | otherwise = (,) Auction { bidsRetracted = username : bidsRetracted, .. } $ BidsRetracted userBidsVal 
  where
      aucEnded = aucMaxBidCount == Li.length bids
      isUserHighestBidder = highestBidder auc == user
      userBidsVal = foldr (\Bid{..} bidsSum -> if bidder == user then bidValue + bidsSum else bidsSum) 0 bids
      retractBids username = Li.filter (\Bid{..} -> bidder /= user)

canRetractBids :: Auction -> Username -> Bool
canRetractBids auc@Auction{..} user@(Username username) = traceShow ("username: " <> username <> " auc in progress " <> show aucInProgress <>   " hasBid " <> show hasBid <> "  | " <> "alreadyRetracted " <> show alreadyRetracted <> " \n") (aucInProgress && hasBid && not (winningBidder == username) && not alreadyRetracted)
    where
      aucStarted = Li.length bids > 0
      aucEnded = aucMaxBidCount == Li.length bids
      aucInProgress = aucStarted && not aucEnded
      hasBid = getUserBidTotal auc username /= 0
      winningBidder = highestBidder auc
      alreadyRetracted = Li.any (== user) bidsRetracted
      
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