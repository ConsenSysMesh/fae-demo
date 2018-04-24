{-# LANGUAGE DuplicateRecordFields #-}

module FaeTX.Outgoing.Types where

import FaeTX.Types

data TXinput
  = FakeBidTXinput Key
                   AucTXID
                   CoinTXID
  | BidTXinput Key
               AucTXID
               CoinTXID
               CoinSCID
               CoinVersion
  | CreateAuctionTXinput Key
                         AucTXID
  | WithdrawCoinTXinput Key
                        AucTXID
  | GetCoinTXinput Key
  | GetMoreCoinsTXinput Key
                        CoinTXID
