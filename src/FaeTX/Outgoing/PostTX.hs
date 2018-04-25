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
getPostTXargs (FakeBidTXinput (Key key) (AucTXID aucTXID) (CoinTXID coinTXID)) =
  formatArgs [("key", show key), ("aucTX", aucTXID), ("coinTX", coinTXID), ("coinSCID", "") , ("coinVersion", "") ] ++
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

-- make sure that dev environment provisioning gives postTX.sh executable permissions
postTX :: [String] -> IO ()
postTX args = readProcess "./contracts/postTX.sh" args [] >>= pPrint

main :: IO ()
main = getCs

getC = postTX $ getPostTXargs (GetCoinTXinput (Key "tom"))
coint = CoinTXID "02a3335a32793f66b102854591ec9614f75a5e7c216b0f24f4dfc5e51db62353"
auct = AucTXID "4109a5409412c8b6b9f54bf3f10eeae36d4885431dee90ea7442e410bd7dd273"
getCs =
  postTX $
  getPostTXargs (GetMoreCoinsTXinput (Key "tom") (coint) )
