{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-------------------------------------------------------------------------------------
  Retrieve the args required to post a transaction to Fae according to the postTX API
--------------------------------------------------------------------------------------}
module PostTX.Outgoing.FormatTX where

import Data.List
import Data.Monoid
import Prelude

import Debug.Trace

import PostTX.Types
import SharedTypes 
import PostTX.Outgoing.Types

type Env = [(String, String)]
type Args = [String]

-- Get the necessary environment variables and args for PostTX in order
-- to post the auction TX
getPostTXopts :: AuctionTXin -> (Env, Args)
getPostTXopts (FakeBidTXin (Key key) (AucTXID aucTXID) (CoinTXID coinTXID)) = (env, ["Bid", "--fake"])
  where env = [("key", key), ("aucTX", aucTXID), ("coinTX", coinTXID), ("coinSCID", ""), ("coinVersion", "")]
getPostTXopts (BidTXin (Key key) (AucTXID aucTXID) (CoinTXID coinTXID) (CoinSCID coinSCID) (CoinVersion coinVersion)) = (env, ["Bid"])
  where env = [ ("key", key), ("aucTX", aucTXID), ("coinTX", coinTXID), ("coinSCID", coinSCID), ("coinVersion", coinVersion) ]
getPostTXopts (CreateAuctionTXin (Key key)) = (env, ["Create"])
  where env = [("key", key)] 
getPostTXopts (WithdrawTXin (Key key) (AucTXID aucTXID)) = (env, ["Withdraw"])
  where env = [("key", key), ("aucTX", aucTXID)]
getPostTXopts (GetCoinTXin (Key key)) = (env, ["GetCoin"])
  where env = [("key", key), ("self", key)] 
getPostTXopts (GetMoreCoinsTXin (Key key) (CoinTXID coinTXID)) = (env, ["GetMoreCoins"])
  where env = [("self", "bidder1"), ("key", "bidder1"), ("coinTX", coinTXID)]
getPostTXopts d = traceShow d ([],[]) 
