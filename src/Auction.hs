{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{--------------------------------------------------------
  Logic to return new Auction State from Auction Actions
---------------------------------------------------------}
module Auction where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Data.Aeson
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D
import qualified Network.WebSockets as WS
import Prelude

import Types

bidOnAuction :: AuctionId -> Bid -> IntMap Auction -> IntMap Auction
bidOnAuction key bid@Bid {..} =
  IntMap.adjust
    (\Auction {..} -> Auction {bids = bid : bids, value = bidValue, ..})
    key

createAuction :: Auction -> IntMap Auction -> IntMap Auction
createAuction auction auctionsMap = IntMap.insert key auction auctionsMap
  where
    key = getNextAuctionKey auctionsMap
    -- auctionStartTimestamp = currentTimeStamp

getNextAuctionKey :: IntMap Auction -> IntMap.Key
getNextAuctionKey a =
  case IntMap.maxViewWithKey a of
    (Just ((k, _), _)) -> k + 1
    Nothing -> 1
