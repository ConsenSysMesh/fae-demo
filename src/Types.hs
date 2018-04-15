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

data ServerState = ServerState
  { clients :: [Client]
  , auctions :: [Auction]
  }

data Auction = Auction
  { createdBy :: String
  , initialValue :: Double
  , maxNumBids :: Int
  , auctionStartTimestamp :: String
  } deriving (Show, Generic, FromJSON)

data Bid = Bid
  { bidValue :: String
  , bidder :: String
  , bidTimestamp :: String
  } deriving (Show, Generic, FromJSON)

-- Actions for synchronising client-server state
data AuctionAction
  = CreateAuctionAction Auction
  | BidAuctionAction Auction
                     Bid
  | IdAuctionAction -- Identity action represents a noop
  deriving (Show, Generic, FromJSON)

instance ToJSON AuctionAction where
  toJSON action = toJSON $ show action

instance ToJSON Bid where
  toJSON bid = toJSON $ show bid

instance Eq Client where
  (Client (x, _)) == (Client (y, _)) = x == y

instance ToJSON Client where
  toJSON (Client (name, _)) = toJSON $ show (name, name)
