{-# LANGUAGE OverloadedStrings #-}

module PostTX where
import Prelude
import System.Process
import Text.Regex.PCRE
import Data.Maybe
import Control.Monad
import Text.Pretty.Simple (pPrint)

txIDregex = "(?<=Transaction\\W)(\\w|\\d)+" :: String

coinVersionRegex = "(?<=versions:\\s      )(\\w|\\d)+" :: String

coinSCIDregex = "(?<=input )(\\w|\\d)+" :: String

tx =
  "Transaction a82b661c0e6a662947bf9d7e271ce23af4980bd25ec04161305baec5e77a349e result : () outputs : [0] signers : self : 0331775 a097e2b85c1f3cb17a802009616dc220b24cb1a2b53168147fe5cf2ca" :: String

data Contract = Bid | Create | Withdraw | GetCoin | GetMoreCoins deriving (Eq, Show)

type TXid = String

type Key = String

type CoinSCID = String

type CoinVersion = String

type CoinTXid = String

type AucTXid = String

type IsFake = Bool

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
postTX :: [String] -> IO TXoutData
postTX args = txOut >>= pPrint >> txOut >>= pure . parseTXoutput
  where
    txOut = readProcess "./loggerPostTX.sh" args []

getFakeArg :: IsFake -> String
getFakeArg fake = if fake then "--fake" else ""

getArgs :: Contract -> Maybe Key -> Maybe AucTXid -> Maybe CoinTXid -> Maybe CoinSCID -> Maybe CoinVersion -> IsFake -> [(String)]
getArgs Bid key aucTXid coinTXid coinSCID coinVersion isFake = ["Bid", getFakeArg isFake]
getArgs Create key _ _ _ _ isFake = ["Create", getFakeArg isFake]
getArgs Withdraw key aucTXid coinTXid coinSCID coinVersion isFake = ["Withdraw", getFakeArg isFake]
getArgs GetCoin _ _ _ _ _  isFake = ["GetCoin", getFakeArg isFake]
getArgs GetMoreCoins _ _ _ _ _  isFake = ["GetMoreCoins", getFakeArg isFake] 

createAuction :: IO TXoutData
createAuction = postTX (getArgs Create Nothing Nothing Nothing Nothing Nothing False)

main = do
  createAuction >>= pPrint

  --postTX contractName args = txOut >>= print >> txOut >>= pure . parseTXoutput
