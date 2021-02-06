{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module EmailServer where

import Control.Monad.IO.Class (liftIO)
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

type EmailServerAPI =
  "api" :> "sms" :> ReqBody '[FormUrlEncoded] IncomingMessage :> Post '[JSON] () :<|>
  "api" :> "ping" :> Get '[JSON] String :<|>
  "api" :> "subscribe" :> Capture "email" Text :> Get '[JSON] ()

pingHandler :: Handler String
pingHandler = return "Pong"

incomingHandler :: IncomingMessage -> Handler ()
incomingHandler (IncomingMessage from body) = liftIO $ do
  putStrLn "Receiving Message"
  print body
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
subscribeEmailHandler email = liftIO $ do
  putStr "Subscribing: "
  print email

emailServerAPI :: Proxy EmailServerAPI
emailServerAPI = Proxy :: Proxy EmailServerAPI

emailServer :: Server EmailServerAPI
emailServer = incomingHandler :<|> pingHandler :<|> subscribeEmailHandler

runServer :: IO ()
runServer = do
  port <- read <$> getEnv "PORT"
  run port (serve emailServerAPI emailServer)
