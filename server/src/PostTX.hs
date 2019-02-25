module PostTX
  ( executeContract
  , updateStartingBid
  , updateMaxBidCount
  , PostTXResponse(..)
  , TXConfig(..)
  ) where

import Data.List

import Debug.Trace

import System.Process

import Control.Monad.Except
import Control.Monad.Reader

import Prelude

import PostTX.Outgoing.PostTX
import PostTX.Types
import SharedTypes

executeContract :: TXConfig -> IO (Either PostTXError PostTXResponse)
executeContract conf@BidConfig {} = execute placeBid conf
executeContract conf@CreateAuctionConfig {} = execute createAuction conf
executeContract conf@GetCoinConfig {} = execute getCoin conf
executeContract conf@GetMoreCoinsConfig {} = execute getMoreCoins conf
executeContract conf@CollectConfig {} = execute collect conf

execute ::
     ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
  -> TXConfig
  -> IO (Either PostTXError PostTXResponse)
execute = runReaderT . runExceptT

-- search and replace on tx message source file
updateStartingBid :: Int -> IO ()
updateStartingBid next = trace cmd (callCommand cmd)
  where 
    sedQuery = concat ["6s/1/", show next, "/"]
    cmd = concat ["gsed -i ", sedQuery, " \"Create.hs\""]

-- search and replace on tx message source file
updateMaxBidCount :: Int -> IO ()
updateMaxBidCount next = trace cmd (callCommand cmd)
  where 
    sedQuery = concat ["7s/4/", show next, "/"]
    cmd = concat ["gsed -i ", sedQuery, " \"Create.hs\""]