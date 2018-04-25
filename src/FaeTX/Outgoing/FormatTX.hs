{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-------------------------------------------------------------------------------------
  Retrieve the args required to post a transaction to Fae according to the postTX API
--------------------------------------------------------------------------------------}
module FaeTX.Outgoing.FormatTX where

import Control.Monad
import Data.List
import Data.Monoid
import Prelude
import System.Process
import Text.Pretty.Simple (pPrint)

import FaeTX.Types

getPostTXargs :: AuctionTX -> [String]
getPostTXargs (FakeBidTX (Key key) (AucTXID aucTXID) (CoinTXID coinTXID)) =
  formatArgs
    [ ("key", show key)
    , ("aucTX", aucTXID)
    , ("coinTX", coinTXID)
    , ("coinSCID", "")
    , ("coinVersion", "")
    ] ++
  ["Bid", "--fake"]
getPostTXargs (BidTX (Key key) (AucTXID aucTXID) (CoinTXID coinTXID) (CoinSCID coinSCID) (CoinVersion coinVersion)) =
  formatArgs
    [ ("key", key)
    , ("aucTX", aucTXID)
    , ("coinTX", coinTXID)
    , ("coinSCID", coinSCID)
    , ("coinVersion", coinVersion)
    ] ++
  ["Bid"]
getPostTXargs (CreateAuctionTX (Key key)) =
  formatArgs [("key", key)] ++ ["Create"]
getPostTXargs (WithdrawCoinTX (Key key) (AucTXID aucTXID)) =
  formatArgs [("key", key), ("aucTX", aucTXID)] ++ ["Withdraw"]
getPostTXargs (GetCoinTX (Key (key))) =
  formatArgs [("key", key), ("self", key)] ++ ["GetCoin"]
getPostTXargs (GetMoreCoinsTX (Key (key)) (CoinTXID (coinTXID))) =
  formatArgs [("self", key), ("key", key), ("coinTX", coinTXID)] ++
  ["GetMoreCoins"]

formatArg :: (String, String) -> [String]
formatArg (key, val) = ["-e"] <> [key <> "=" <> val]

formatArgs :: [(String, String)] -> [String]
formatArgs = concatMap formatArg
