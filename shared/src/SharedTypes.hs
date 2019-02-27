
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module SharedTypes where

import Data.Aeson.Types
import Data.Time.Clock
import Data.Time.Calendar
import GHC.Generics
import Data.Map.Lazy (Map)
import  qualified Data.Map.Lazy as M

import FaeTypes
import FaeCrypto
import FaeJSON

import Data.Aeson
import Data.Aeson.TH
import qualified Data.Map as Map


newtype Key =
  Key String
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype Username =
    Username String
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type CoinCount = Int
    
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
  , aucMaxBidCount :: Int
  , bidsRetracted :: [Username]
  , bidsRefunded :: [Username]
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

-- encode first two invariant for bid retractions 
--  1. auction is in progress
--  2. given username has placed bids
--  3. bids haven't already been retracted by username

-- second set of invariants would be that bids cannot be refunded unless
-- 1. auction has ended
-- 2. given username has placed bids
-- 3. given username doesnt have the highest bid
-- 4. bids haven't already been refunded to username


data Bid = Bid
  { bidValue :: Int
  , bidder :: String
  , bidTimestamp :: UTCTime
  , isWinningBid :: Bool
  } deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON)

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
  | CollectRequest AucTXID-- incoming
  | CollectionSubmitted String Username UTCTime CoinCollection AucTXID Auction -- outgoing
  | AuctionCreated Username AucTXID
                   Auction -- outgoing
  | RequestCoins Int -- incoming 
  | CoinsGenerated String Username UTCTime Int -- outgoing
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

-- Represents a user getting their coins back from an auction either through bid cancellation or losing the auction
data CoinCollection = LoserRefunded CoinCount | BidsRetracted CoinCount | CoinCollectionErr CollectErr
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

data CollectErr = HighBidderCan'tCollect | UserNotBidded | AuctionNotStarted
    deriving (Show, Eq, Generic, FromJSON, ToJSON)
