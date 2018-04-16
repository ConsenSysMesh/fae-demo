{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import Data.Aeson.Types
import Data.Text (Text)
import GHC.Generics
import qualified Network.WebSockets as WS

--We represent a client by their username and a `WS.Connection`. We will see how we
--obtain this `WS.Connection` later on.
newtype Client =
  Client (Text, WS.Connection)

instance Show Client where
  show (Client (name, _)) = show name

data ServerState = ServerState
  { clients :: [Client]
  , auctions :: [Auction]
  } deriving (Show)

data Auction = Auction
  { createdBy :: String
  , initialValue :: Double
  , maxNumBids :: Int
  , auctionStartTimestamp :: String
  } deriving (Show, Generic, FromJSON, ToJSON)

data Bid = Bid
  { auctionId :: String
  , bidValue :: String
  , bidder :: String
  , bidTimestamp :: String
  } deriving (Show, Generic, FromJSON, ToJSON)

-- Actions for synchronising client-server state
data AuctionAction
  = CreateAuctionAction Auction
  | BidAuctionAction Bid
  deriving (Show, Generic, FromJSON, ToJSON)

instance Eq Client where
  (Client (x, _)) == (Client (y, _)) = x == y

instance ToJSON Client where
  toJSON (Client (name, _)) = toJSON $ show (name, name)
