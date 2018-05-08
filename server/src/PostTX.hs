module PostTX
  ( executeContract
  , module PostTX.Types
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import PostTX.Outgoing.PostTX
import Prelude
import PostTX.Types

executeContract :: TXConfig -> IO (Either PostTXError PostTXResponse)
executeContract conf@BidConfig {} = execute bid conf
executeContract conf@CreateAuctionConfig {} = execute createAuction conf
executeContract conf@GetCoinConfig {} = execute getCoin conf
executeContract conf@GetMoreCoinsConfig {} = execute getMoreCoins conf
executeContract conf@WithdrawConfig {} = execute withdraw conf

execute ::
     ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
  -> TXConfig
  -> IO (Either PostTXError PostTXResponse)
execute = runReaderT . runExceptT
