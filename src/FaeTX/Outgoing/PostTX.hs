{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FaeTX.Outgoing.PostTX where

import Control.Monad
import FaeTX.Outgoing.Types
import Prelude
import System.IO
import System.Process
import Text.Pretty.Simple (pPrint)

getPostTXArgs :: TXinput -> [String]
getPostTXArgs (FakeBidTXinput key aucTXID coinTXID) = ["--fake", "Bid"]
getPostTXArgs (BidTXinput key aucTXID coinTXID coinSCID coinVersion) = ["Bid"]
getPostTXArgs (CreateAuctionTXinput key aucTXID) = ["Create"]
getPostTXArgs (WithdrawCoinTXinput key aucTXID) = ["Withdraw"]
getPostTXArgs (GetCoinTXinput key) = ["-e self=" ++ show key, "GetCoin"]
getPostTXArgs (GetMoreCoinsTXinput key coinTXID) = ["GetMoreCoins"]

-- make sure that dev environment provisioning gives postTX.sh executable permissions
postTX :: [String] -> IO ()
postTX args = do
  (_, Just hout, _, _) <-
    createProcess (proc "./contracts/postTX.sh" args) {std_out = CreatePipe}
  txOutput <- hGetContents hout
  putStrLn $ "> posttx args " ++ (show args)
  putStrLn txOutput

getFakeArg :: Bool -> String
getFakeArg fake =
  if fake
    then "--fake"
    else ""

main :: IO ()
main = postTX ["Create"]
