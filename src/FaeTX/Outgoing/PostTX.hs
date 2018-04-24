{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FaeTX.Outgoing.PostTX where

import Control.Monad
import Data.List
import Data.Monoid
import FaeTX.Outgoing.Types
import Prelude
import System.Process
import Text.Pretty.Simple (pPrint)

import FaeTX.Types

getPostTXargs :: TXinput -> [String]
getPostTXargs (FakeBidTXinput key aucTXID coinTXID) =
  formatArgs
    [("key", show key), ("aucTX", show aucTXID), ("coinTX", show coinTXID)] ++
  ["--fake", "Bid"]
getPostTXargs (BidTXinput key aucTXID coinTXID coinSCID coinVersion) =
  formatArgs
    [ ("key", show key)
    , ("aucTX", show aucTXID)
    , ("coinTX", show coinTXID)
    , ("coinSCID", show coinSCID)
    , ("coinVersion", show coinVersion)
    ] ++
  ["Bid"]
getPostTXargs (CreateAuctionTXinput key aucTXID) =
  formatArgs [("key", show key), ("aucTX", show aucTXID)] ++ ["Create"]
getPostTXargs (WithdrawCoinTXinput key aucTXID) =
  formatArgs [("key", show key), ("aucTX", show aucTXID)] ++ ["Withdraw"]
getPostTXargs (GetCoinTXinput (Key (key))) =
  formatArgs [("key", show key), ("self", show key)] ++ ["GetCoin"]
getPostTXargs (GetMoreCoinsTXinput (Key (key)) (CoinTXID (coinTXID))) =
  formatArgs [("key", show key),("self", show key), ("coinTX", show coinTXID)] ++ ["GetMoreCoins"]

formatArg :: (String, String) -> [String]
formatArg (key, val) = ["-e"] <> [key <> "=" <> val]

formatArgs :: [(String, String)] -> [String]
formatArgs = concatMap formatArg

-- make sure that dev environment provisioning gives postTX.sh executable permissions
postTX :: [String] -> IO ()
postTX args = readProcess "./contracts/postTX.sh" args [] >>= pPrint

getFakeArg :: Bool -> String
getFakeArg fake =
  if fake
    then "--fake"
    else ""

main :: IO ()
main = postTX $ getPostTXargs (GetMoreCoinsTXinput (Key "tom") (CoinTXID "f6d974df9a7f2721f7f56f87d5cf6694d97c761df817ad6db47ccf91a960922b"))
