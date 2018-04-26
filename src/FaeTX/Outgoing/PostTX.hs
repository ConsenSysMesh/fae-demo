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
import Debug.Trace
import Prelude
import System.Process
import Text.Pretty.Simple (pPrint)
import System.IO


import System.IO
import System.Process

postTX tx = do
    (_, Just hout, Just herr, jHandle) <-
        -- Replace with some other command on Windows
        createProcess (proc "./contracts/postTX.sh" (getPostTXargs tx))
           { cwd = Just "."
           , std_out = CreatePipe
           , std_err = CreatePipe 
           }

    putStrLn "First line of stdout:"
    hGetContents hout >>= putStrLn
    hGetContents herr >>= putStrLn

    exitCode <- waitForProcess jHandle
    putStrLn $ "Exit code: " ++ show exitCode
    txout <- hGetContents hout
    print txout

    return txout

 {-
-- make sure that dev environment provisioning gives postTX.sh executable permissions
postTX :: AuctionTXin -> IO String
postTX tx = readProcess "./contracts/postTX.sh" args []
  where
    args = getPostTXargs tx
    -}
