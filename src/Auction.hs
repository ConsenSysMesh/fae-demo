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

bidOnAuction :: Auction -> Bid -> Auction
bidOnAuction Auction {..} Bid {..} = undefined

createAuction ::
     Auction -> IntMap AuctionId Auction -> IntMap Auction Id Auction
createAuction auction auctionsMap = IntMap.insert key auction auctionsMap
  where
    key = getNextAuctionId auctionsMap

getNextAuctionId :: IntMap Auction Id Auction -> AuctionId
getNextAuctionId =
  case IntMap.lookupMax auctionsMap of
    Nothing -> 1
    (Just k) -> key + 1
