{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FaeTX.Outgoing.PostTX where

import Control.Monad
import FaeTX.Outgoing.Types
import Prelude
import System.Process
import Text.Pretty.Simple (pPrint)
import Data.List
import Data.Monoid

getPostTXArgs :: TXinput -> [String]
getPostTXArgs (FakeBidTXinput key aucTXID coinTXID) = 
  formatArgs [("key", show key), ("aucTX", show aucTXID), ("coinTX", show coinTXID)] ++ ["--fake", "Bid"]
getPostTXArgs (BidTXinput key aucTXID coinTXID coinSCID coinVersion) = 
  formatArgs [("key", show key), ("aucTX", show aucTXID), ("coinTX", show coinTXID), ("coinSCID", show coinSCID), ("coinVersion", show coinVersion)] ++ ["Bid"]
getPostTXArgs (CreateAuctionTXinput key aucTXID) =
  formatArgs [("key", show key), ("aucTX", show aucTXID)] ++ ["Create"]
getPostTXArgs (WithdrawCoinTXinput key aucTXID) =
  formatArgs [("key", show key), ("aucTX", show aucTXID)] ++ ["Withdraw"]
getPostTXArgs (GetCoinTXinput key) =
  formatArgs [("key", show key)] ++ ["GetCoin"]
getPostTXArgs (GetMoreCoinsTXinput key coinTXID) = 
  formatArgs [("key", show key), ("coinTX", show coinTXID)] ++ ["GetMoreCoins"]

prependEnvArg :: String -> [String]
prependEnvArg arg = ["-e"] <> [arg]

quoteString :: String -> String
quoteString str = "\"" <> str <> "\""

formatArg :: (String, String) -> [String]
formatArg (key, val) = prependEnvArg $ key <> "=" <> quoteString val

formatArgs :: [(String, String)] -> [String]
formatArgs args = concatMap formatArg args

-- make sure that dev environment provisioning gives postTX.sh executable permissions
postTX :: [String] -> IO ()
postTX args = readProcess "./contracts/postTX.sh" args [] >>= pPrint

getFakeArg :: Bool -> String
getFakeArg fake =
  if fake
    then "--fake"
    else ""

main :: IO ()
main = postTX ["-e","self=\"tom\"", "GetCoin"]
