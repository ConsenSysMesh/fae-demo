
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module SharedTypes where

import Data.Aeson.Types
import Data.Time.Clock
import Data.Time.Calendar
import GHC.Generics

import FaeTypes
import FaeCrypto
import FaeJSON

newtype Key =
  Key String
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
  -- id of the tx which created auction

newtype Username =
    Username String
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

    
newtype UTCTimestamp =
    UTCTimestamp UTCTime
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- id of tx which created coin
newtype CoinTXID =
  CoinTXID String
  deriving (Show, Eq, Generic, ToJSON, FromJSON, Ord)

newtype AucTXID =
  AucTXID String
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON, Ord) -- Int represents the number of the argument that failed

data Auction = Auction
  { bids :: [Bid]
  , createdBy :: String
  , createdTimestamp :: UTCTime
  , startingValue :: Int
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Bid = Bid
  { bidValue :: Int
  , bidder :: String
  , bidTimestamp :: UTCTime
  , isWinningBid :: Bool
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

data AuctionOpts = AuctionOpts
 {  startingVal :: Int
  , maxBidCount :: Int
}  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Msg
  = CreateAuctionRequest AuctionOpts -- incoming
  | BidRequest AucTXID -- incoming
               Int
  | BidSubmitted String AucTXID -- outgoing
                 Bid
  | AuctionCreated Username AucTXID
                   Auction -- outgoing
  | RequestCoins Int -- incoming 
  | CoinsGenerated Int -- outgoing
  | ErrMsg Err
  deriving (Show, Generic, FromJSON, ToJSON)

data Err
  = PostTXErr PostTXError -- outgoing
  | NoCoins -- outgoing
   deriving (Show, Generic, FromJSON, ToJSON)

data PostTXError
  = TXFailed String
  | TXBodyFailed String
  | TXSummaryParseFailed String
  | TXInputFailed Int
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
