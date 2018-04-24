{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

--{-# LANGUAGE DuplicateRecordFields #-}
module Types where

import Data.Aeson.Types
import Data.IntMap.Lazy (IntMap)
import Data.Text (Text)
import Data.Time.Clock
import GHC.Generics
import qualified Network.WebSockets as WS

type AuctionId = Int

--We represent a client by their username and a `WS.Connection`. We will see how we
--obtain this `WS.Connection` later on.
newtype Client =
  Client (Text, WS.Connection) -- add wallet

instance Show Client where
  show (Client (name, _)) = show name

data ServerState = ServerState
  { clients :: [Client]
  , auctions :: IntMap Auction
  } deriving (Show)

data Auction = Auction
  { auctionId :: AuctionId
  , bids :: [Bid]
  , createdBy :: String
  , initialValue :: Int
  , maxNumBids :: Int
  , createdTimestamp :: UTCTime
  } deriving (Show, Generic, FromJSON, ToJSON)

data Bid = Bid
  { bidValue :: Int
  , bidder :: String
  , bidTimestamp :: UTCTime
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Eq Client where
  (Client (x, _)) == (Client (y, _)) = x == y
  -- Actions for synchronising client-server state
