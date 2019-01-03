module Coins
  ( generateCoins
  ) where

import Control.Monad
import Control.Monad.Except
import Data.Either
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Prelude

import PostTX
import Types

import SharedTypes
import FaeFrontend
import FaeCrypto

generateCoins :: Key -> Int -> Wallet -> ExceptT PostTXError IO Wallet
generateCoins key numCoins w@(Wallet wallet)
  | Map.null wallet && numCoins == 1 = depositCoin key w
  | Map.null wallet && numCoins > 1 = do
    postTXResult <- lift $ getCoin key
    either
      throwError
      (\(GetCoinTX txid) -> depositCoins key w numCoins (CoinTXID (show txid)))
      postTXResult
  | otherwise = do
    postTXResult <- lift $ getCoins key baseCoinTXID numCoins -- todo instead - call getmorecoins on previous cache and then updatewallet int is sum of old and new coins
    either
      throwError
      (\(GetMoreCoinsTX txid) -> do
         let baseCoinCacheValue = fromJust $ Map.lookup baseCoinTXID wallet
         let newCoinCacheValue = (numCoins + baseCoinCacheValue)
         return $
           Wallet $
           Map.insert
             (CoinTXID (show txid))
             newCoinCacheValue
             (Map.delete baseCoinTXID wallet))
      postTXResult
  where baseCoinTXID = fst $ head $ Map.toList wallet

depositCoins ::
     Key -> Wallet -> Int -> CoinTXID -> ExceptT PostTXError IO Wallet
depositCoins key wallet numCoins coinTXID = do
  postTXResponse <- liftIO (getCoins key coinTXID numCoins)
  either
    throwError
    (\(GetMoreCoinsTX txid) -> 
      return $ deposit wallet numCoins (CoinTXID (show txid)))
    postTXResponse

depositCoin :: Key -> Wallet -> ExceptT PostTXError IO Wallet
depositCoin key wallet = do
  postTXResponse <- liftIO (getCoin key)
  either
    throwError
    (\(GetCoinTX txid) -> return $ deposit wallet numCoins (CoinTXID (show txid)))
    postTXResponse
  where
    numCoins = 1

getCoin :: Key -> IO (Either PostTXError PostTXResponse)
getCoin key = executeContract (GetCoinConfig key)

getCoins :: Key -> CoinTXID -> Int -> IO (Either PostTXError PostTXResponse)
getCoins key coinTXID@(CoinTXID txid) numCoins
  | numCoins == 0 = return (Right (GetMoreCoinsTX (read txid)))
  | otherwise = do
    (Right (GetMoreCoinsTX txid)) <- liftIO getMoreCoins
    getCoins key (CoinTXID (show txid)) (numCoins - 1)
  where
    getMoreCoins = executeContract (GetMoreCoinsConfig key coinTXID)

deposit :: Wallet -> Int -> CoinTXID -> Wallet
deposit (Wallet wallet) numCoins coinTXID =
  Wallet $ Map.insert coinTXID numCoins wallet
