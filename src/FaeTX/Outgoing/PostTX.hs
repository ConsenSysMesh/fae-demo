{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FaeTX.Outgoing.PostTX where

import Control.Monad
import Data.Maybe
import Prelude
import System.Process
import Text.Pretty.Simple (pPrint)

getPostTXArgs :: TXinput -> [Strings]
getPostTXArgs (FakeBidTXinput key aucTXID coinTXID coinSCID coinVersion) =
  [getFakeArg isFake, "Bid"]
getPostTXArgs (BidTXinput key aucTXID coinTXID coinSCID coinVersion) =
  [getFakeArg isFake, "Bid"]
getPostTXArgs (CreateAuctionTXinput key aucTXID) = ["Create"]
getPostTXArgs (WithdrawCoinTXinput key aucTXID) = ["Withdraw"]
getPostTXArgs (GetCoinTXinput key) = ["-e key=" ++ key, "GetCoin"]
getPostTXArgs (GetMoreCoinsTXinput key coinTXID) = ["GetMoreCoins"]

-- make sure that dev environment provisioning gives postTX.sh executable permissions
postTX :: TXinput -> String
postTX args = txOut >>= pPrint >> txOut >>= pure . parseTXoutput
  where
    txOut = readProcess "./contracts/postTX.sh" args []

getFakeArg :: Bool -> String
getFakeArg fake =
  if fake
    then "--fake"
    else ""
{-
getArgs ::
     Contract
  -> Maybe Key
  -> Maybe AucTXID
  -> Maybe CoinTXID
  -> Maybe CoinSCID
  -> Maybe CoinVersion
  -> Fake
  -> [(String)]
getArgs Bid key aucTXID coinTXID coinSCID coinVersion isFake =
  [getFakeArg isFake, "Bid"]
getArgs Create key a = ["Create"]
getArgs Withdraw key aucTXID coinTXID coinSCID coinVersion _ = ["Withdraw"]
getArgs GetCoin (Just key) _ _ _ _ _ = ["-e key=" ++ key, "GetCoin"]
getArgs GetMoreCoins key _ _ _ _ _ = ["GetMoreCoins"]


createAuction :: IO CreateAuctionTXout
createAuction =
  postTX (getArgs Create Nothing Nothing Nothing Nothing Nothing False)

getCoin :: Key -> IO GetCoinTXout
getCoin key =
  postTX (getArgs GetCoin (Just key) Nothing Nothing Nothing Nothing False)

main :: IO ()
main = do
  getCoin "tom" >>= pPrint
-}
