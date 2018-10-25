{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-------------------------------------------------------------------------------------
  Retrieve the args required to post a transaction to Fae according to the postTX API
--------------------------------------------------------------------------------------}
module PostTX.Outgoing.FormatTX where

import Data.List
import Data.Monoid
import Prelude

import PostTX.Types
import SharedTypes 
import PostTX.Outgoing.Types

getPostTXargs :: AuctionTXin -> [String]
getPostTXargs (FakeBidTXin (Key key) (AucTXID aucTXID) (CoinTXID coinTXID)) =
  formatArgs
    [ ("key", key)
    , ("aucTX", aucTXID)
    , ("coinTX", coinTXID)
    , ("coinSCID", "")
    , ("coinVersion", "")
    ] ++
  ["Bid", "--fake"]
getPostTXargs (BidTXin (Key key) (AucTXID aucTXID) (CoinTXID coinTXID) (CoinSCID coinSCID) (CoinVersion coinVersion)) =
  formatArgs
    [ ("key", key)
    , ("aucTX", aucTXID)
    , ("coinTX", coinTXID)
    , ("coinSCID", coinSCID)
    , ("coinVersion", coinVersion)
    ] ++
  ["Bid"]
getPostTXargs (CreateAuctionTXin (Key key)) =
  formatArgs [("key", key)] ++ ["Create"]
getPostTXargs (WithdrawTXin (Key key) (AucTXID aucTXID)) =
  formatArgs [("key", key), ("aucTX", aucTXID)] ++ ["Withdraw"]
getPostTXargs (GetCoinTXin (Key key)) =
  formatArgs [("key", key), ("self", key)] ++ ["GetCoin"]
getPostTXargs (GetMoreCoinsTXin (Key key) (CoinTXID coinTXID)) =
  formatArgs [("self", "bidder1"), ("key", "bidder1"), ("coinTX", coinTXID)] ++
  ["GetMoreCoins"]

formatArg :: (String, String) -> [String]
formatArg (key, val) = ["-e"] <> [key <> "=" <> val]

formatArgs :: [(String, String)] -> [String]
formatArgs = concatMap formatArg
