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
    [("key", show key), ("aucTX", aucTXID), ("coinTX", coinTXID)] ++
  ["--fake", "Bid"]
getPostTXargs (BidTXinput key aucTXID coinTXID coinSCID coinVersion) =
  formatArgs
    [ ("key", key)
    , ("aucTX", aucTXID)
    , ("coinTX", coinTXID)
    , ("coinSCID", coinSCID)
    , ("coinVersion", coinVersion)
    ] ++
  ["Bid"]
getPostTXargs (CreateAuctionTXinput (Key key) (AucTXID aucTXID)) =
  formatArgs [("key", key), ("aucTX", aucTXID)] ++ ["Create"]
getPostTXargs (WithdrawCoinTXinput (Key key) (AucTXID aucTXID)) =
  formatArgs [("key", key), ("aucTX", aucTXID)] ++ ["Withdraw"]
getPostTXargs (GetCoinTXinput (Key (key))) =
  formatArgs [("key", key), ("self", key)] ++ ["GetCoin"]
getPostTXargs (GetMoreCoinsTXinput (Key (key)) (CoinTXID (coinTXID))) =
  formatArgs [("self",  key), ("key",  key), ("coinTX",  coinTXID)] ++ ["GetMoreCoins"]

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
main = getCs
getC = postTX $ getPostTXargs (GetCoinTXinput (Key "tom"))
  
getCs = postTX $ getPostTXargs (GetMoreCoinsTXinput (Key "tom") (CoinTXID "e207e26d7a14dcfc8881d63b292ed8cd3e6170c49c8c2a516d0e946ebed51930"))
