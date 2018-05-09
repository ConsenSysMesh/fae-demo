{-# LANGUAGE DuplicateRecordFields #-}

module PostTX.Incoming.Types where

import PostTX.Types
import SharedTypes

type IsWinningBid = Bool

data FakeBidTXout = FakeBidTXout
  { key :: Key
  , txid :: TXID
  , aucTXID :: AucTXID
  , coinTXID :: CoinTXID
  , coinSCID :: CoinSCID
  , coinVersion :: CoinVersion
  }

data BidTXout = BidTXout
  { txid :: TXID
  , aucTXID :: AucTXID
  , coinTXID :: CoinTXID
  , coinSCID :: CoinSCID
  , coinVersion :: CoinVersion
  , isWinningBid :: IsWinningBid
  }

data AuctionTXout
  = CreateAuctionTXout TXID
  | WithdrawTXout TXID
  | GetCoinTXout TXID
  | GetMoreCoinsTXout TXID
  deriving (Show, Eq)
