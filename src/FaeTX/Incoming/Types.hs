{-# LANGUAGE DuplicateRecordFields #-}

module FaeTX.Incoming.Types where

type TXID = String

type Key = String

type CoinSCID = String

type CoinVersion = String

type CoinTXID = String

type AucTXID = String

data BidTXout = BidTXout
  { txId :: TXID
  , key :: Key
  , aucTXID :: AucTXID
  , coinTXID :: CoinTXID
  , coinsSCID :: CoinSCID
  , coinVersion :: CoinVersion
  }

data CreateAuctionTXout = CreateAuctionTXout
  { txId :: TXID
  , key :: Key
  , aucTXID :: AucTXID
  }

data WithdrawCoinTXout = WithdrawCoinTXout
  { txId :: TXID
  , key :: Key
  }

data GetCoinTXout = GetCoinTXout
  { txId :: TXID
  , key :: Key
  }

data GetMoreCoinsTXout = GetMoreCoinsTXout
  { txId :: TXID
  , key :: Key
  }
