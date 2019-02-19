{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module PostTX.Types where

import SharedTypes (Key, AucTXID, CoinTXID, PostTXError)
import FaeFrontend

newtype AucStartingValue = AucStartingValue Int deriving (Show, Eq)

newtype MaxBidCount = MaxBidCount Int deriving (Show, Eq)

-- gives us the output of the transaction call so we can identify and 
-- respond to invalid calls such as bidding below the min bid.
newtype TXResult = TXResult String deriving (Show, Eq)

data PostTXResponse
  = AuctionCreatedTX TransactionID AucStartingValue MaxBidCount
  | BidTX TransactionID
        AucTXID
        CoinTXID
        TXResult -- change to result
  | GetCoinTX TransactionID
  | GetMoreCoinsTX TransactionID
  | WithdrawTX TransactionID
  | Collect TransactionID
  deriving (Show, Eq)

data TXConfig
  = BidConfig Key
              AucTXID
              CoinTXID
  | CreateAuctionConfig Key AucStartingValue MaxBidCount
  | GetCoinConfig Key
  | GetMoreCoinsConfig Key CoinTXID
  | WithdrawConfig Key AucTXID
  | CollectConfig Key AucTXID
