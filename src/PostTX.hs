{-# LANGUAGE OverloadedStrings #-}

module PostTX where

import Control.Monad
import Data.Maybe
import Prelude
import System.Process
import Text.Pretty.Simple (pPrint)
import Text.Regex.PCRE

data Contract
  = Bid
  | Create
  | Withdraw
  | GetCoin
  | GetMoreCoins
  deriving (Eq, Show)

type TXid = String

type Key = String

type CoinSCID = String

type CoinVersion = String

type CoinTXid = String

type AucTXid = String

type IsFake = Bool

-- Every TX has an ID but only some have coinSCID or coinVersion
data TXoutData = TXoutData
  { txId :: TXid
  , coinsSCID :: Maybe (CoinSCID)
  , coinVersion :: Maybe (CoinVersion)
  } deriving (Eq, Show)

txIDregex :: String
txIDregex = "(?<=Transaction\\W)(\\w|\\d)+" :: String

coinVersionRegex :: String
coinVersionRegex = "(?<=versions:\\s      )(\\w|\\d)+" :: String

coinSCIDregex :: String
coinSCIDregex = "(?<=input )(\\w|\\d)+" :: String

-----------------------------------------------------------------------------
-- Parsing Output of postTX.sh
-----------------------------------------------------------------------------
parseTXid :: String -> String
parseTXid str = str =~ txIDregex :: String

parseCoinVersion :: String -> Maybe String
parseCoinVersion str
  | result == "" = Nothing
  | otherwise = Just result
  where
    result = str =~ coinVersionRegex :: String

parseCoinSCID :: String -> Maybe String
parseCoinSCID str
  | result == "" = Nothing
  | otherwise = Just result
  where
    result = str =~ coinSCIDregex :: String

parseTXoutput :: String -> TXoutData
parseTXoutput txOut =
  TXoutData
    { txId = parseTXid txOut
    , coinsSCID = parseCoinSCID txOut
    , coinVersion = parseCoinVersion txOut
    }

-- make sure that dev environment provisioning gives postTX.sh executable permissions
postTX :: [String] -> IO TXoutData
postTX args = txOut >>= pPrint >> txOut >>= pure . parseTXoutput
  where
    txOut = readProcess "./contracts/postTX.sh" args []

getFakeArg :: IsFake -> String
getFakeArg fake =
  if fake
    then "--fake"
    else ""

getArgs ::
     Contract
  -> Maybe Key
  -> Maybe AucTXid
  -> Maybe CoinTXid
  -> Maybe CoinSCID
  -> Maybe CoinVersion
  -> IsFake
  -> [(String)]
getArgs Bid key aucTXid coinTXid coinSCID coinVersion isFake =
  [getFakeArg isFake, "Bid"]
getArgs Create _ _ _ _ _ _ = ["Create"]
getArgs Withdraw key aucTXid coinTXid coinSCID coinVersion _ = ["Withdraw"]
getArgs GetCoin (Just key) _ _ _ _ _ = ["-e key=" ++ key, "GetCoin"]
getArgs GetMoreCoins key _ _ _ _ _ = ["GetMoreCoins"]

createAuction :: IO TXoutData
createAuction =
  postTX (getArgs Create Nothing Nothing Nothing Nothing Nothing False)

getCoin :: Key -> IO TXoutData
getCoin key =
  postTX (getArgs GetCoin (Just key) Nothing Nothing Nothing Nothing False)

main :: IO ()
main = do
  getCoin "tom" >>= pPrint
