{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server
import System.Environment (getEnv)

import SMS
import Mailchimp

type ServerAPI = "api" :> "ping" :> Get '[JSON] String :<|>
  "api" :> "sms" :> ReqBody '[FormUrlEncoded] IncomingMessage :> Post '[JSON] () :<|>
  "api" :> "subscribe" :> Capture "email" Text :> Get '[JSON] ()

pingHandler :: Handler String
pingHandler = return "Pong"

smsHandler :: IncomingMessage -> Handler ()
smsHandler msg = 
  case messageToCommand (body msg) of
    Nothing -> liftIO $ sendMessage (fromNumber msg) "Sorry, we didn't understand that request!"
    Just (SubscribeCommand email) -> do
      _ <- sendSubscribeEmail email
      return ()

subscribeHandler :: Text -> Handler ()
subscribeHandler email = do
  listId <- fetchListId 
  case listId of
    Left _ -> error "Failed to find list ID!"
    Right listId' -> do
      _ <- subscribeUser listId' (Subscriber email)
      return ()

emailList :: (Text, ByteString, Maybe ByteString) -> Handler ()
emailList content = do
  listId <- fetchListId 
  case listId of
    Left _ -> error "Failed to find list ID!"
    Right listId' -> do
      subscribers <- fetchListMembers listId'
      case subscribers of
        Left _ -> error "Failed to find subscribers!"
        Right subscribers' -> do
          _ <- sendEmailToList content (subscriberEmail <$> subscribers')
          return ()

twilioAPI :: Proxy ServerAPI
twilioAPI = Proxy :: Proxy ServerAPI

twilioServer :: Server ServerAPI
twilioServer = pingHandler :<|> smsHandler :<|> subscribeHandler

runServer :: IO ()
runServer = do
  port <- read <$> getEnv "PORT"
  run port (serve twilioAPI twilioServer)
