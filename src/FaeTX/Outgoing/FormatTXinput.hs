{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FaeTX.Outgoing.FormatTXinput where

import Control.Monad
import Data.List
import Data.Monoid
import FaeTX.Outgoing.Types
import Prelude
import System.Process
import Text.Pretty.Simple (pPrint)

import FaeTX.Types

getPostTXargs :: TXinput -> [String]
getPostTXargs (FakeBidTXinput (Key key) (AucTXID aucTXID) (CoinTXID coinTXID)) =
  formatArgs
    [ ("key", show key)
    , ("aucTX", aucTXID)
    , ("coinTX", coinTXID)
    , ("coinSCID", "")
    , ("coinVersion", "")
    ] ++
  ["Bid", "--fake"]
getPostTXargs (BidTXinput (Key key) (AucTXID aucTXID) (CoinTXID coinTXID) (CoinSCID coinSCID) (CoinVersion coinVersion)) =
  formatArgs
    [ ("key", key)
    , ("aucTX", aucTXID)
    , ("coinTX", coinTXID)
    , ("coinSCID", coinSCID)
    , ("coinVersion", coinVersion)
    ] ++
  ["Bid"]
getPostTXargs (CreateAuctionTXinput (Key key)) =
  formatArgs [("key", key)] ++ ["Create"]
getPostTXargs (WithdrawCoinTXinput (Key key) (AucTXID aucTXID)) =
  formatArgs [("key", key), ("aucTX", aucTXID)] ++ ["Withdraw"]
getPostTXargs (GetCoinTXinput (Key (key))) =
  formatArgs [("key", key), ("self", key)] ++ ["GetCoin"]
getPostTXargs (GetMoreCoinsTXinput (Key (key)) (CoinTXID (coinTXID))) =
  formatArgs [("self", key), ("key", key), ("coinTX", coinTXID)] ++
  ["GetMoreCoins"]

formatArg :: (String, String) -> [String]
formatArg (key, val) = ["-e"] <> [key <> "=" <> val]

formatArgs :: [(String, String)] -> [String]
formatArgs = concatMap formatArg
