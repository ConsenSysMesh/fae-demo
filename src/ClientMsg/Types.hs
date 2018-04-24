{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ClientMsg.Types where

import Data.Aeson.Types
import Data.IntMap.Lazy (IntMap)
import Data.Text (Text)
import Data.Time.Clock
import GHC.Generics
import qualified Network.WebSockets as WS
import Types

-- Actions for synchronising client-server state
data AuctionAction
  = CreateAuctionAction Auction
  | BidAuctionAction AuctionId
                     Bid
  deriving (Show, Generic, FromJSON, ToJSON)
