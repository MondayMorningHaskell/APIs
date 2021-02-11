{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module FullServer where

import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy (Proxy(..))
import Data.Text (pack, Text)
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server
import System.Environment
import Twilio (runTwilio')
import Twilio.Messages (post, PostMessage(..))

import Email
import SMS
import Subscribers

type FullServerAPI =
  "api" :> "sms" :> ReqBody '[FormUrlEncoded] IncomingMessage :> Post '[JSON] () :<|>
  "api" :> "ping" :> Get '[JSON] String :<|>
  "api" :> "subscribe" :> Capture "email" Text :> Get '[JSON] ()

pingHandler :: Handler String
pingHandler = return "Pong"

incomingHandler :: IncomingMessage -> Handler ()
incomingHandler (IncomingMessage from body) = liftIO $ do
  case messageToCommand body of
    Nothing -> do
      twilioNum <- fetchTwilioNumber
      runTwilio' fetchSid fetchToken $ do
        let body = "Sorry, we didn't understand that request!"
        let newMessage = PostMessage from twilioNum body Nothing
        _ <- post newMessage
        return ()
    Just (SubscribeCommand email) -> sendSubscribeEmail email

subscribeEmailHandler :: Text -> Handler ()
subscribeEmailHandler email = do
  listName <- pack <$> liftIO (getEnv "MAILCHIMP_LIST_NAME")
  listId <- tryIO (fetchMailchimpListId listName)
  tryIO (subscribeMailchimpMember listId email)

tryIO :: IO (Either String a) -> Handler a
tryIO action = do
  result <- liftIO action
  case result of
    Left e -> throwM $ err500 { errBody = BSL.fromStrict $ BSC.pack (show e)}
    Right x -> return x

finalServerAPI :: Proxy FullServerAPI
finalServerAPI = Proxy :: Proxy FullServerAPI

finalServer :: Server FullServerAPI
finalServer = incomingHandler :<|> pingHandler :<|> subscribeEmailHandler

runServer :: IO ()
runServer = do
  port <- read <$> getEnv "PORT"
  run port (serve finalServerAPI finalServer)
