{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

--{-# LANGUAGE DuplicateRecordFields #-}
module Types where

import Data.Aeson.Types
import Data.Map.Lazy (Map)
import Data.Monoid
import Data.Text (Text)
import Data.Time.Clock
import FaeTX.Post
import GHC.Generics
import qualified Network.WebSockets as WS

data Client = Client
  { name :: Text
  , conn :: WS.Connection
  , wallet :: Wallet
  }

instance Show Client where
  show Client {..} = show name <> show wallet

data ServerState = ServerState
  { clients :: [Client]
  , auctions :: Map AucTXID Auction
  } deriving (Show)

data Auction = Auction
  { bids :: [Bid]
  , createdBy :: String
  , createdTimestamp :: UTCTime
  } deriving (Show, Generic, FromJSON, ToJSON)

data Msg
  = CreateAuctionRequest -- incoming
  | BidRequest AucTXID -- incoming
               Int
  | BidSubmitted AucTXID -- outgoing
                 Bid
  | AuctionCreated AucTXID
                   Auction -- outgoing
  | RequestCoins Int -- incoming 
  | CoinsGenerated Int -- outgoing 
  | ErrMsg PostTXError -- outgoing
  deriving (Show, Generic, FromJSON, ToJSON)

data Bid = Bid
  { bidValue :: Int
  , bidder :: String
  , bidTimestamp :: UTCTime
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Eq Client where
  Client {name = name1} == Client {name = name2} = name1 == name2

newtype Wallet =
  Wallet (Map CoinTXID Int) -- Int is balance for coin cache
  deriving (Show, Eq)
