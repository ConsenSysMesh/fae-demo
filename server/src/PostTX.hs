module PostTX
  ( executeContract
  , updateStartingBid
  , updateMaxBidCount
  , PostTXResponse(..)
  , TXConfig(..)
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import PostTX.Outgoing.PostTX
import Prelude
import PostTX.Types
import SharedTypes

executeContract :: TXConfig -> IO (Either PostTXError PostTXResponse)
executeContract conf@BidConfig {} = execute placeBid conf
executeContract conf@CreateAuctionConfig {} = execute createAuction conf
executeContract conf@GetCoinConfig {} = execute getCoin conf
executeContract conf@GetMoreCoinsConfig {} = execute getMoreCoins conf
executeContract conf@WithdrawConfig {} = execute withdraw conf

execute ::
     ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
  -> TXConfig
  -> IO (Either PostTXError PostTXResponse)
execute = runReaderT . runExceptT

-- search and replace on tx message source file
updateStartingBid :: Int -> Int -> String
updateStartingBid prev next = concat ["gsed -i ", sedQuery, " \"Create.hs\""]
  where sedQuery = concat ["6s/", show prev, "/", show next, "/"]

-- search and replace on tx message source file
updateMaxBidCount :: Int -> Int -> String
updateMaxBidCount prev next = concat ["gsed -i ", sedQuery, " \"Create.hs\""]
  where sedQuery = concat ["7s/", show prev, "/", show next, "/"]
