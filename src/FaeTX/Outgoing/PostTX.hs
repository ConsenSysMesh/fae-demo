{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FaeTX.Outgoing.PostTX where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.List
import Data.Monoid
import FaeTX.Incoming.Types
import FaeTX.Outgoing.FormatTX
import FaeTX.Outgoing.Types
import FaeTX.Types
import System.Exit
import System.IO
import System.Process

postTX ::
     AuctionTXin
  -> ExceptT PostTXError (ReaderT TXConfig IO) (ExitCode, String, String)
postTX tx = do
  (exitCode, stdOut, stdErr) <- liftIO $ readProcessWithExitCode path args []
  liftIO $ System.IO.putStrLn stdOut
  liftIO $ System.IO.putStrLn stdErr
  return (exitCode, stdOut, stdErr)
  where
    args = getPostTXargs tx
    path = "./contracts/postTX.sh"
