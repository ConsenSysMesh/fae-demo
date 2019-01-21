
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Proxy
import Servant.API
import Servant.Utils.Links
import Miso
import Miso.String
import qualified Miso.String as S

import Model
import Views
import Types

main :: IO ()
main = runApp

runApp :: IO ()
runApp = do
  startApp App {model=getInitialModel initialURI, initialAction = AppAction goLogin, ..}
  where
    initialURI = linkURI $ safeLink (Proxy :: Proxy API) (Proxy :: Proxy Login)
    events = defaultEvents
    subs = [websocketSub uri protocols (AppAction . HandleWebSocket), uriSub (AppAction . HandleURI) ]
    update = updateModel
    view = appView
    uri = URL "ws://localhost:9160"
    protocols = Protocols []
    mountPoint = Nothing