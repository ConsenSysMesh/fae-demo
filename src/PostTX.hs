{-# LANGUAGE OverloadedStrings #-}

module PostTX where

import System.Process
import Text.Regex.PCRE

txIDregex = "(?<=Transaction\\W)(\\w|\\d)+" :: String

coinVersionRegex = "(?<=versions:\\s      )(\\w|\\d)+" :: String

coinSCIDregex = "(?<=input )(\\w|\\d)+" :: String

tx =
  "Transaction a82b661c0e6a662947bf9d7e271ce23af4980bd25ec04161305baec5e77a349e result : () outputs : [0] signers : self : 0331775 a097e2b85c1f3cb17a802009616dc220b24cb1a2b53168147fe5cf2ca" :: String

type ContractName = String

type TXid = String

type CoinSCID = String

type CoinVersion = String

type CoinTX = String

type AucTX = String

-----------------------------------------------------------------------------
-- Parsing Output of postTX.sh
-----------------------------------------------------------------------------
-- Every TX has an ID but only some have coinSCID or coinVersion
data TXoutData = TXoutData
  { txId :: TXid
  , coinsSCID :: Maybe (CoinSCID)
  , coinVersion :: Maybe (CoinVersion)
  } deriving (Eq, Show)

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
postTX :: ContractName -> [String] -> IO TXoutData
postTX contractName args = txOut >>= pure . parseTXoutput
  where
    txOut = readProcess "./postTX.sh" args []

main = do
  postTX " " [] >>= print
