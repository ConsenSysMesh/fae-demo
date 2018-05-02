module FaeOutgoing.Coins (generateCoins) where

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

import Data.Maybe
import Data.Text (Text)
import FaeTX.Post
import qualified Network.WebSockets as WS
import Prelude
import Text.Pretty.Simple (pPrint)
import Types
import FaeTX.Types

generateCoins :: Key -> Int -> Wallet -> IO Wallet
generateCoins key numCoins (Wallet wallet) 
  | Map.null wallet && numCoins == 1 = do 
    (Right coinTXID) <- getCoin key
    return $ Wallet $ Map.insert coinTXID numCoins wallet
  | Map.null wallet && numCoins > 1  = do
        (Right initialCoinTXID) <- getCoin key
        (Right finalCoinTXID) <- getCoins key initialCoinTXID (numCoins -1)
        return $ Wallet $ Map.insert finalCoinTXID numCoins wallet
  | otherwise                = do  
        (Right coinTXID) <- getCoins key firstCoinTXID numCoins
        return $ Wallet $  Map.insert coinTXID numCoins wallet
            where firstCoinTXID = fst $ head $ Map.toList wallet

getCoins :: Key -> CoinTXID -> Int -> IO (Either PostTXError CoinTXID)
getCoins key coinTXID numCoins | numCoins == 0 = return (Right coinTXID)
                                    | otherwise     = do
                                            (Right (GetMoreCoins (TXID txid))) <- getMoreCoins
                                            getCoins key (CoinTXID txid) (numCoins - 1)
    where getMoreCoins = executeContract (GetMoreCoinsConfig key coinTXID)

getCoin :: Key -> IO (Either PostTXError CoinTXID)
getCoin key = do 
    (Right (GetCoin (TXID txid))) <- executeContract (GetCoinConfig key)
    return (Right (CoinTXID txid))