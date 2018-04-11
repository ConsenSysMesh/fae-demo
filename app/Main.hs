{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where
import Prelude
import Data.Char (isPunctuation, isSpace)
import Data.Monoid
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.Encoding as D
import qualified Data.Text.Lazy as X
import qualified Network.WebSockets as WS
import Data.Aeson
import GHC.Generics

data Bid = Bid {
    value :: String
  , bidder :: String
  , timestamp :: String
  } | Noop deriving (Show, Generic)
  
instance FromJSON Bid
instance ToJSON Bid where
    toJSON bid = toJSON $ show bid

--We represent a client by their username and a `WS.Connection`. We will see how we
--obtain this `WS.Connection` later on.

newtype Client = Client (Text, WS.Connection)

instance Eq Client where
  (Client (x, _)) == (Client (y, _)) = x == y

--The state kept on the server is simply a list of connected clients. We've added
--an alias and some utility functions, so it will be easier to extend this state
--later on.

type ServerState = [Client]

instance ToJSON Client where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toJSON (Client (name, _)) = toJSON $ show (name, name)

--Create a new, initial state:

newServerState :: ServerState
newServerState = []

--Get the number of active clients:

numClients :: ServerState -> Int
numClients = length

--Check if a user already exists (based on username):

clientExists :: Client -> ServerState -> Bool
clientExists client clients = elem client clients

--Add a client (this does not check if the client already exists, you should do
--this yourself using `clientExists`):

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

--Remove a client:

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter (/= client)

--Send a message to all clients, and log it on stdout:

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    T.putStrLn message
    forM_ clients $ \(Client (_, conn)) -> WS.sendTextData conn message


--The main function first creates a new state for the server, then spawns the
--actual server. For this purpose, we use the simple server provided by
--`WS.runServer`.

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9140 $ application state

--Our main application has the type:

application :: MVar ServerState -> WS.ServerApp

--Note that `WS.ServerApp` is nothing but a type synonym for
--`WS.PendingConnection -> IO ()`.

--Our application starts by accepting the connection. In a more realistic
--application, you probably want to check the path and headers provided by the
--pending request.

--We also fork a pinging thread in the background. This will ensure the connection
--stays alive on some browsers.

application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30

--When a client is succesfully connected, we read the first message. This should
--be in the format of "Hi! I am Jasper", where Jasper is the requested username.

    msg <- WS.receiveData conn
    clients <- readMVar state
    case msg of

--Check that the first message has the right format:

     --   _   | not (prefix `T.isPrefixOf` msg) ->
      --          WS.sendTextData conn ("Wrong announcement" :: Text)

--Check the validity of the username:

      --      | any ($ clientName)
       --        [T.null, T.any isPunctuation, T.any isSpace] ->
        --            WS.sendTextData conn ("Name cannot " `mappend`
         --               "contain punctuation or whitespace, and " `mappend`
          --              "cannot be empty" :: Text)

--Check that the given username is not already taken:

         _  | clientExists client clients ->
                WS.sendTextData conn ("User already exists" :: Text)

--All is right! We're going to allow the client, but for safety reasons we *first*
--setup a `disconnect` function that will be run when the connection is closed.

            | otherwise -> flip finally disconnect $ do

--We send a "Welcome!", according to our own little protocol. We add the client to
--the list and broadcast the fact that he has joined. Then, we give control to the
--'talk' function.

               modifyMVar_ state $ \s -> do
                   let s' = addClient client s
                   T.putStrLn clientName
                   print jsonClient
                   WS.sendTextData conn $ T.pack "nnu"
                   broadcast jsonBid s'
                   return s'
               talk conn state client
          where
            prefix     = "Hi! I am "
            clientName = T.filter  (\c -> c `notElem` ['"',' ']) $  T.drop (T.length prefix) msg
            client     = Client (clientName, conn)
            sampleBid  = Bid{ value="1", bidder="Xena", timestamp="12pm" }
            jsonClient =  X.toStrict $ D.decodeUtf8 $ encode client
            jsonBid = X.toStrict $ D.decodeUtf8 $ encode sampleBid
            disconnect = do
                -- Remove client and return new state
                s <- modifyMVar state $ \s ->
                    let s' = removeClient client s in return (s', s')
                broadcast (clientName `mappend` " disconnected") s

--The talk function continues to read messages from a single client until he
--disconnects. All messages are broadcasted to the other clients.

talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
talk conn state (Client(user, _)) = forever $ do
    msg <- WS.receiveData conn
    readMVar state >>= broadcast
        (msg)
