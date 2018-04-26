{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{----------------------------------------------
  Api for High level fae auction TXs management
-----------------------------------------------}
module FaeTX.Main where

import Control.Monad
import Data.List
import Data.Monoid
import Debug.Trace
import FaeTX.Incoming.ParseTX
import FaeTX.Incoming.Types
import FaeTX.Outgoing.FormatTX
import FaeTX.Outgoing.PostTX
import FaeTX.Outgoing.Types
import Prelude
import System.Process
import Text.Pretty.Simple (pPrint)

import FaeTX.Types

placeFakeBid :: Key -> AucTXID -> CoinTXID -> IO (Maybe AuctionTXout)
placeFakeBid key aucTXID coinTXID =
  postTX fakeBid >>= pure . (fakeBidParser key aucTXID coinTXID)
  where
    fakeBid = FakeBidTXin key aucTXID coinTXID

getBidTXin :: AuctionTXout -> AuctionTXin
getBidTXin (FakeBidTXout key _ aucTXID coinTXID coinSCID coinVersion) =
  (BidTXin (key) (aucTXID) (coinTXID) (coinSCID) (coinVersion))

placeBid :: AuctionTXout -> IO (Maybe AuctionTXout)
placeBid fBid@(FakeBidTXout key _ aucTXID coinTXID _ _) =
  (postTX $ getBidTXin fBid) >>= pure . bidParser key aucTXID coinTXID

bid key auctionid coinid =
  (placeFakeBid key auctionid coinid) >>= \fBid ->
    case fBid of
      Just fbidOut -> placeBid fbidOut
      Nothing -> pure Nothing
    
createAuction key = (postTX (CreateAuctionTXin key)) >>= pure .  createAuctionParser >>= print

getCoin key = postTX (GetCoinTXin key) >>= pure . getCoinParser

-- take the coins from an old cache destroy the cache and deposit the old coins + 1 new coin to a new cache
getMoreCoins key coinTXID = postTX (GetMoreCoinsTXin key coinTXID) >>= pure . getCoinParser

withdraw key aucTXID = postTX (GetMoreCoinsTXin key coinTXID) >>= pure . getCoinParser

--main = 
main = bid key auctionid coinid >>= print

  --getMoreCoins key coinid

--getCoin key >>= print
key = Key "bidder1"

auctionid =
  AucTXID "e673db9705c9c85e65e0fe6e9f9b2eb13195c244daf4cb36e8aa48c223780804"

coinid =
  CoinTXID "ba176290c888b8a3f9146052ef07507d54d996fee94d79f36062ac93cd69f84e"

 -- 9cf4103511c46ca2b0ee255c8368b63d02cd66f66e9fb6c3c439fd9569ad170b
 -- 6041899bdfccf2ec1008a1c9f7fe323254c6fbf067fd794f210742712ccda2b0
 -- edc29dfeb8fe80ac348b59658b67a53cad2a0b25dd0951f572b170e0c4e36bc1


callFaeContract txInput txOutParser = postTx txInput >>= pure . TxOutParser

getTXinput :: AuctionContract -> AuctionTXin
getTXinput = undefined


 -- exposed funcs

postToFae :: AuctionContract -> PostTXResponse
postToFae = undefined