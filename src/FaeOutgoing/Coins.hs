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
import Control.Monad.Except
import Data.Either

generateCoins :: Key -> Int -> Wallet -> ExceptT PostTXError IO Wallet
generateCoins key numCoins (Wallet wallet) 
  | Map.null wallet && numCoins == 1 = do 
      postTXResponse <- liftIO getCoin
      either throwError (\(GetCoin (TXID txid)) -> return $  Wallet $  Map.insert (CoinTXID txid) numCoins wallet) postTXResponse
  | Map.null wallet && numCoins > 1  = do
        postTXResponse1 <- liftIO  getCoin
        either throwError (\(GetCoin (TXID txid)) -> do
            postTXResponse2 <- liftIO $ getCoins key (CoinTXID txid) numCoins
            (either throwError (\(GetCoin (TXID txid)) -> return $  Wallet $  Map.insert (CoinTXID txid) numCoins wallet)  postTXResponse2)) postTXResponse1
  | otherwise   = do  
        postTXResponse <- liftIO $ getCoins key firstCoinTXID numCoins
        either throwError (\(GetCoin (TXID txid)) -> return $  Wallet $  Map.insert (CoinTXID txid) numCoins wallet) postTXResponse
          where firstCoinTXID = fst $ head $ Map.toList wallet
                getCoin = (executeContract (GetCoinConfig key))

getCoins :: Key -> CoinTXID -> Int -> IO (Either PostTXError PostTXResponse)
getCoins key coinTXID@(CoinTXID txid) numCoins | numCoins == 0 = return (Right (GetMoreCoins (TXID txid)))
                                    | otherwise = do
                                            (Right (GetMoreCoins (TXID txid))) <- liftIO (getMoreCoins)
                                            getCoins key (CoinTXID txid) (numCoins - 1)
    where getMoreCoins = executeContract (GetMoreCoinsConfig key coinTXID)

