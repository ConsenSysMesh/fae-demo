{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Shared where

import Data.Aeson.Types
import Data.Map.Lazy (Map)
import Data.Map.Lazy
import Data.Monoid
import Data.Time.Clock
import GHC.Generics
import PostTX

--import PostTX
data Auction = Auction
  { bids :: [Bid]
  , createdBy :: String
  , createdTimestamp :: UTCTime
  } deriving (Show, Generic, FromJSON, ToJSON)

data Bid = Bid
  { bidValue :: Int
  , bidder :: String
  , bidTimestamp :: UTCTime
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

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
  | ErrMsg Err
  deriving (Show, Generic, FromJSON, ToJSON)

data Err
  = PostTXErr PostTXError -- outgoing
  | NoCoins -- outgoing
   deriving (Show, Generic, FromJSON, ToJSON)
