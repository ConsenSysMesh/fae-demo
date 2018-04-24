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

import FaeTX.Types

getPostTXargs :: TXinput -> [String]
getPostTXargs (FakeBidTXinput key aucTXID coinTXID) = 
  f [("key", show key), ("aucTX", show aucTXID), ("coinTX", show coinTXID)] ++ ["--fake", "Bid"]
getPostTXargs (BidTXinput key aucTXID coinTXID coinSCID coinVersion) = 
  f  [("key", show key), ("aucTX", show aucTXID), ("coinTX", show coinTXID), ("coinSCID", show coinSCID), ("coinVersion", show coinVersion)] ++ ["Bid"]
getPostTXargs (CreateAuctionTXinput key aucTXID) =
 f [("key", show key), ("aucTX", show aucTXID)] ++ ["Create"]
getPostTXargs (WithdrawCoinTXinput key aucTXID) =
  f [("key", show key), ("aucTX", show aucTXID)] ++ ["Withdraw"]
getPostTXargs (GetCoinTXinput key) =
  f  [("key", show key)] ++ ["GetCoin"]
getPostTXargs (GetMoreCoinsTXinput key coinTXID) = 
  f [("key", show key), ("coinTX", show coinTXID)] ++ ["GetMoreCoins"]

prependEnvArg :: String -> [String]
prependEnvArg arg = ["-e"] <> [arg]

quoteString :: String -> String
quoteString str =  "\"" <> str <> "\""

formatArg :: (String, String) -> [String]
formatArg (key, val) =[ key <> "=" <> (quoteString val)]

f = concatMap formatArg
-- make sure that dev environment provisioning gives postTX.sh executable permissions
postTX :: [String] -> IO ()
postTX args = readProcess "./contracts/postTX.sh" args [] >>= pPrint

getFakeArg :: Bool -> String
getFakeArg fake =
  if fake
    then "--fake"
    else ""

main :: IO ()
main = print $ getPostTXargs (GetCoinTXinput (Key "tom"))
