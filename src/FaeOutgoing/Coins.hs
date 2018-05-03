module FaeOutgoing.Coins
  ( generateCoins
  ) where

----------------------------------------
-- Post Coin Transactions to Fae
----------------------------------------
import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Foldable
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import Control.Monad.Except
import Data.Either
import Data.Maybe
import Data.Text (Text)
import FaeTX.Post
import FaeTX.Types
import qualified Network.WebSockets as WS
import Prelude
import Text.Pretty.Simple (pPrint)
import Types

generateCoins :: Key -> Int -> Wallet -> ExceptT PostTXError IO Wallet
generateCoins key numCoins w@(Wallet wallet)
  | Map.null wallet && numCoins == 1 = depositCoin key w
  | Map.null wallet && numCoins > 1 = do
      postTXResult <- lift $ getCoin key
      either
        throwError
        (\(GetCoin (TXID txid)) -> depositCoins key w numCoins (CoinTXID txid)) postTXResult
  | otherwise = depositCoins key w numCoins firstCoinTXID -- todo instead - call getmorecoins on previous cache and then updatewallet int is sum of old and new coins
  where
    firstCoinTXID = fst $ head $ Map.toList wallet

depositCoins ::
     Key -> Wallet -> Int -> CoinTXID -> ExceptT PostTXError IO Wallet
depositCoins key wallet numCoins coinTXID = do
  postTXResponse <- liftIO (getCoins key coinTXID numCoins)
  either
    throwError
    (\(GetMoreCoins (TXID txid)) -> return $ depositCoins (CoinTXID txid))
    postTXResponse
  where
    depositCoins = deposit wallet numCoins

depositCoin :: Key -> Wallet -> ExceptT PostTXError IO Wallet
depositCoin key wallet = do
  postTXResponse <- liftIO (getCoin key)
  either
    throwError
    (\(GetCoin (TXID txid)) -> return $ depositCoins (CoinTXID txid))
    postTXResponse
  where
    numCoins = 1
    depositCoins = deposit wallet numCoins

getCoin :: Key -> IO (Either PostTXError PostTXResponse)
getCoin key = executeContract (GetCoinConfig key)

getCoins :: Key -> CoinTXID -> Int -> IO (Either PostTXError PostTXResponse)
getCoins key coinTXID@(CoinTXID txid) numCoins
  | numCoins == 0 = return (Right (GetMoreCoins (TXID txid)))
  | otherwise = do
    (Right (GetMoreCoins (TXID txid))) <- liftIO getMoreCoins
    getCoins key (CoinTXID txid) (numCoins - 1)
  where
    getMoreCoins = executeContract (GetMoreCoinsConfig key coinTXID)

deposit :: Wallet -> Int -> CoinTXID -> Wallet
deposit (Wallet wallet) numCoins coinTXID =
  Wallet $ Map.insert coinTXID numCoins wallet
