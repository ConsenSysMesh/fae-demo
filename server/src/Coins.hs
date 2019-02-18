{-# LANGUAGE TupleSections #-}

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

import Control.Monad.Trans.State.Lazy
import SharedTypes
import FaeFrontend
import FaeCrypto

generateCoins :: Key -> Int -> Wallet -> ExceptT PostTXError (StateT Wallet IO) TransactionID
generateCoins key numCoins w@(Wallet wallet)
  | Map.null wallet && numCoins == 1 = depositCoin key w
  | Map.null wallet && numCoins > 1 = do
    postTXResult <- lift $ liftIO $ getCoin key
    either
      throwError
      (\(GetCoinTX txid) -> depositCoins key w numCoins (CoinTXID (show txid)))
      postTXResult
  | otherwise = do
    postTXResult <- lift $ liftIO $ getCoins key baseCoinTXID numCoins -- todo instead - call getmorecoins on previous cache and then updatewallet int is sum of old and new coins
    either
      throwError
      (\(GetMoreCoinsTX txid) -> do
         let baseCoinCacheValue = fromJust $ Map.lookup baseCoinTXID wallet
         let newCoinCacheValue = numCoins + baseCoinCacheValue
         let newWallet = Wallet $ Map.insert (CoinTXID (show txid)) newCoinCacheValue (Map.delete baseCoinTXID wallet)
         lift $ put newWallet
         return txid)
      postTXResult
  where baseCoinTXID = fst $ head $ Map.toList wallet

depositCoins :: Key -> Wallet -> Int -> CoinTXID -> ExceptT PostTXError (StateT Wallet IO) TransactionID
depositCoins key wallet numCoins coinTXID = do
  postTXResponse <- liftIO (getCoins key coinTXID numCoins)
  either
    throwError
    (\(GetMoreCoinsTX txid) -> do
      lift $ put $ deposit wallet numCoins (CoinTXID $ show txid)
      return txid)
    postTXResponse

depositCoin :: Key -> Wallet -> ExceptT PostTXError (StateT Wallet IO) TransactionID
depositCoin key wallet = do
  postTXResponse <- liftIO (getCoin key)
  either
    throwError
    (\(GetCoinTX txid) -> do
      lift $ put $ deposit wallet numCoins (CoinTXID (show txid))
      return txid)
    postTXResponse
  where
    numCoins = 1

getCoin :: Key -> IO (Either PostTXError PostTXResponse)
getCoin key = putStrLn "\n get coin \n">> executeContract (GetCoinConfig key)

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
