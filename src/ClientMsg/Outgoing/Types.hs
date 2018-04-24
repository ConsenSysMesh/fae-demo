module ClientMsg.Outgoing.Types where

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
  | WithdrawCoinTXinput Key
                        AucTXID
  | GetCoinTXinput Key
  | GetMoreCoinsTXinput Key
