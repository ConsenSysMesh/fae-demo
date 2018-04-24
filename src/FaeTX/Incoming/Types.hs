{-# LANGUAGE DuplicateRecordFields #-}

module FaeTX.Incoming.Types where

import FaeTX.Types


data TXOutput
  = FakeBidTXoutput TXID
                    AucTXID
                    CoinTXID
  | BidTXoutput TXID
                AucTXID
                CoinTXID
                CoinSCID
                CoinVersion
  | CreateAuctionTXoutput TXID
                          AucTXID
  | WithdrawCoinTXoutput TXID
                         AucTXID
  | GetCoinTXoutput TXID
  | GetMoreCoinsTXoutput TXID
                         CoinTXID
