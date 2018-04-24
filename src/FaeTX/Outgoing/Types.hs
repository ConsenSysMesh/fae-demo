{-# LANGUAGE DuplicateRecordFields #-}

module FaeTX.Outgoing.Types where

type Key = String -- private key for signing txs

type CoinSCID = String -- hash of the coin

type CoinVersion = String

type CoinTXID = String -- id of tx which created coin

type AucTXID = String -- id of the tx which created auction

data FakeBidTXin = FakeBidTXinput
  { key :: Key
  , aucTXID :: AucTXID
  , coinTXID :: CoinTXID
  }

data BidTXin = BidTXinput
  { key :: Key
  , aucTXID :: AucTXID
  , coinTXID :: CoinTXID
  , coinsSCID :: CoinSCID
  , coinVersion :: CoinVersion
  }

data CreateAuctionTXin = CreateAuctionTXinput
  { key :: Key
  , aucTXID :: AucTXID
  }

data WithdrawCoinTXin = WithdrawCoinTXinput
  { aucTXID :: AucTXID
  , key :: Key
  }

data GetCoinTXin =
  GetCoinTXinput Key

data GetMoreCoinsTXin = GetMoreCoinsTXinput
  { coinTXID :: CoinTXID
  , key :: Key
  }
