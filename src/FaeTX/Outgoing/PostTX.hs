{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FaeTX.Outgoing.PostTX where

import Control.Monad
import Data.List
import Data.Monoid
import FaeTX.Incoming.Types
import FaeTX.Outgoing.FormatTX
import FaeTX.Outgoing.Types
import FaeTX.Types
import System.Process
import System.IO
import System.Exit

postTX :: AuctionTXin -> IO (ExitCode, String, String)
postTX tx = do
    (exitCode, stdOut, stdErr) <- readProcessWithExitCode path args []
    System.IO.putStrLn stdOut
    System.IO.putStrLn stdErr
    return (exitCode, stdOut, stdErr) 
    where args = getPostTXargs tx 
          path = "./contracts/postTX.sh"
