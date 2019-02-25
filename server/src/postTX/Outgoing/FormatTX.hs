{-# LANGUAGE OverloadedStrings #-}
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

type ContractName = String
type Env = [(String, String)]
type Args = [String]

data PostTXOpts = PostTXOpts { 
  contractName :: String,
  env :: [(String, String)],
  args :: [String]
} deriving (Show)

-- Get the necessary environment variables and args for PostTX in order
-- to post the auction TX
getPostTXopts (BidTXin (Key key) (AucTXID aucTXID) (CoinTXID coinTXID)) = 
  PostTXOpts { contractName = "Bid", args = [], env = env}
  where env = [ ("key", key), ("self", key), ("aucTX", aucTXID), ("coinTX", coinTXID) ]

getPostTXopts (CreateAuctionTXin (Key key)) =
  PostTXOpts { contractName = "Create", args = ["--","--json"], env = env}
  where env = [("key", key),  ("self", key)]

getPostTXopts (WithdrawTXin (Key key) (AucTXID aucTXID)) = 
  PostTXOpts { contractName = "Collect", args = [], env = env}
  where env = [("key", key), ("aucTX", aucTXID)]

getPostTXopts (GetCoinTXin (Key key)) =
  PostTXOpts { contractName = "GetCoin", args = ["--","--json"], env = env}
  where env = [("key", key), ("self", key), ("ver", "Current")] 

getPostTXopts (GetMoreCoinsTXin (Key key) (CoinTXID coinTXID)) = 
  PostTXOpts { contractName = "GetMoreCoins", args = ["--","--json"], env = env}
  where env = [("ver", "Current"), ("self", key), ("key", key), ("coinTX", coinTXID)]

getPostTXopts (CollectTXin (Key key) (AucTXID aucTXID)) = 
  PostTXOpts { contractName = "Collect", args = ["--","--json"], env = env}
  where env = [("key", key), ("aucTX", aucTXID)]
