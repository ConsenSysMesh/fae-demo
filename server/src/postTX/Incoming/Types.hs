{-# LANGUAGE DuplicateRecordFields #-}

module PostTX.Incoming.Types where

import PostTX.Types
import SharedTypes
import FaeFrontend

type IsWinningBid = Bool

data BidTXout = BidTXout
  { txid :: TransactionID
  , aucTXID :: AucTXID
  , coinTXID :: CoinTXID
  , isWinningBid :: IsWinningBid
  }

data AuctionTXout
  = CreateAuctionTXout TransactionID
  | WithdrawTXout TransactionID
  | GetCoinTXout TransactionID
  | GetMoreCoinsTXout TransactionID
  deriving (Show, Eq)
