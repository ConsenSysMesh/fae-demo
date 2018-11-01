{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PostTX.Outgoing.PostTX where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Data.List

import Prelude

import System.Exit
import System.IO
import System.Process
import System.Environment
import System.Directory

import Debug.Trace

import PostTX.Types
import PostTX.Incoming.ParseTX
import PostTX.Incoming.Types
import PostTX.Outgoing.FormatTX
import PostTX.Outgoing.Types
import SharedTypes

postTX ::
     AuctionTXin
  -> ExceptT PostTXError (ReaderT TXConfig IO) (ExitCode, String, String)
postTX tx = do
  faeHome <- liftIO $ getEnv "FAE_HOME_DIR"
  let finalEnv = ("HOME", faeHome) : env
  liftIO $ print finalEnv
  liftIO $ print finalArgs
  (exitCode, stdOut, stdErr) <- liftIO $ withCurrentDirectory "/Users/tom/code/teamfae/demo/auction-server/server/contracts" (readProcessWithExitCode "stack" (["exec", "postTX"] ++ traceShow finalArgs finalArgs) "")
  --(exitCode, stdOut, stdErr) <- liftIO $ readCreateProcessWithExitCode (shell $ unwords ("stack exec postTX" : "--" : finalArgs)){ cwd = pure "/Users/tom/code/teamfae/demo/auction-server/server/contracts", std_out = CreatePipe, env = pure finalEnv } ""
  liftIO $ putStrLn stdOut
  liftIO $ putStrLn stdErr
  liftIO $ print exitCode
  return (exitCode, stdOut, stdErr)
  where
    PostTXOpts {..} = traceShow (getPostTXopts tx) (getPostTXopts tx)
    finalArgs = contractName : "--" : args ++ ["--json"]
{-  (exitCode, stdOut, stdErr) <- liftIO $ readProcessWithExitCode cmd args []
  liftIO $ System.IO.putStrLn stdOut
  liftIO $ System.IO.putStrLn stdErr
  return (exitCode, stdOut, stdErr)
  where
    args = traceShow (getPostTXargs tx ++ ["--json"]) (getPostTXargs tx  ++ ["--json"])
    cmd = "stack exec postTX"
    -}

bid :: ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
bid = do
  (FakeBidTX _ _ coinTXID coinSCID coinVersion) <- placeFakeBid
  placeBid coinTXID coinSCID coinVersion

placeFakeBid :: ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
placeFakeBid = do
  config@(BidConfig key aucTXID coinTXID) <- ask
  (exitCode, stdOut, stdErr) <- postTX (FakeBidTXin key aucTXID coinTXID)
  case exitCode of
    ExitSuccess ->
      maybe
        (throwError $ TXBodyFailed stdOut)
        (\FakeBidTXout {..} ->
           return $ FakeBidTX key aucTXID coinTXID coinSCID coinVersion)
        (runReaderT (fakeBidParser stdOut) config)
    ExitFailure _ -> throwError (TXFailed stdErr)

placeBid ::
     CoinTXID
  -> CoinSCID
  -> CoinVersion
  -> ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
placeBid coinTXID coinSCID coinVersion = do
  config@(BidConfig key aucTXID coinTXID) <- ask
  (exitCode, stdOut, stdErr) <-
    postTX (BidTXin key aucTXID coinTXID coinSCID coinVersion)
  case exitCode of
    ExitSuccess ->
      maybe
        (throwError $ TXBodyFailed stdOut)
        (\BidTXout {..} -> return $ BidTX txid aucTXID coinTXID isWinningBid)
        (runReaderT (bidParser stdOut) config)
    ExitFailure _ -> throwError $ TXFailed stdErr

createAuction :: ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
createAuction = do
  (CreateAuctionConfig key) <- ask
  (exitCode, stdOut, stdErr) <- postTX (CreateAuctionTXin key)
  case exitCode of
    ExitSuccess ->
      maybe
        (throwError $ TXBodyFailed stdOut)
        (\(CreateAuctionTXout txid) -> return $ AuctionCreatedTX txid)
        (createAuctionParser stdOut)
    ExitFailure _ -> throwError $ TXFailed stdErr

getCoin :: ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
getCoin = do
  (GetCoinConfig key) <- ask
  (exitCode, stdOut, stdErr) <- postTX (GetCoinTXin key)
  case exitCode of
    ExitSuccess ->
      maybe
        (throwError $ TXBodyFailed stdOut)
        (\(GetCoinTXout txid) -> return $ GetCoinTX txid)
        (getCoinParser stdOut)
    ExitFailure _ -> throwError $ TXFailed stdErr

-- take the coins from an old cache destroy the cache and deposit the old coins + 1 new coin to a new cache
getMoreCoins :: ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
getMoreCoins = do
  (GetMoreCoinsConfig key coinTXID) <- ask
  (exitCode, stdOut, stdErr) <- postTX (GetMoreCoinsTXin key coinTXID)
  case exitCode of
    ExitSuccess ->
      maybe
        (throwError $ TXBodyFailed stdOut)
        (\(GetMoreCoinsTXout txid) -> return $ GetMoreCoinsTX txid)
        (getMoreCoinsParser stdOut)
    ExitFailure _ -> throwError $ TXFailed stdErr

withdraw :: ExceptT PostTXError (ReaderT TXConfig IO) PostTXResponse
withdraw = do
  (WithdrawConfig key aucTXID) <- ask
  (exitCode, stdOut, stdErr) <- postTX (WithdrawTXin key aucTXID)
  case exitCode of
    ExitSuccess ->
      maybe
        (throwError $ TXBodyFailed stdOut)
        (\(WithdrawTXout txid) -> return $ WithdrawTX txid)
        (withdrawParser stdOut)
    ExitFailure _ -> throwError $ TXFailed stdErr
