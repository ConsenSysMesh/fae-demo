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
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D
import qualified Network.WebSockets as WS
import Prelude

import Types

bidOnAuction :: Auction -> Bid -> Auction
bidOnAuction (BidAuctionAction Auction {..} Bid {..} = undefined

endAuction ::

handleAuctionAction :: AuctionAction -> Auction
handleAuctionAction
 (CreateAuctionAction auction) = auction
handleAuctionAction
handleAuctionAction
 (BidAuctionAction Auction {..} Bid {..}) = bidOnAuction auction bid
handleAuctionAction
 IdAuctionAction auction = auction

