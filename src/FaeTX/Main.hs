{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{----------------------------------------------
  Api for High level fae auction TXs management
-----------------------------------------------}
module FaeTX.Main where
import Debug.Trace
import Control.Monad
import Data.List
import Data.Monoid
import FaeTX.Incoming.ParseTX
import FaeTX.Incoming.Types
import FaeTX.Outgoing.FormatTX
import FaeTX.Outgoing.Types
import FaeTX.Outgoing.PostTX
import Prelude
import System.Process
import Text.Pretty.Simple (pPrint)

import FaeTX.Types

callContract :: AuctionTXin -> IO String
callContract (GetCoinTXin key) = postTX (GetCoinTXin key)
callContract (CreateAuctionTXin key) = postTX (CreateAuctionTXin key)
callContract (WithdrawCoinTXin key aucTXID) = postTX (WithdrawCoinTXin key aucTXID)
callContract (FakeBidTXin key aucTXID coinTXID) = postTX (FakeBidTXin key aucTXID coinTXID)

placeFakeBid key aucTXID coinTXID = callContract fakeBid >>= print . (fakeBidParser aucTXID coinTXID)
    where fakeBid = FakeBidTXin key aucTXID coinTXID

placeBid key (FakeBidTXout txid aucTXID coinTXID coinSCID coinVersion) = callContract (BidTXin
  (key)
  (aucTXID)
  (coinTXID)
      (coinSCID)
      (coinVersion))


--bid key aucTXID coinTXID = placeFakeBid key aucTXID coinTXID >>= placeBid key

--main =  bid k aucid cointxid

k = Key "bidder1"
aucid =  AucTXID "ea4e069cdd8e2367415d735d083b9dc31c94f1e30f7fcc3f667dd13144e419c1"
cointxid = CoinTXID "864407287131a97c3ee07cb6aab4c6a6cae9ae6a80e381055393256b1adb387b"