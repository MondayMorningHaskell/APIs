{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module SMSServer where

import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy(..))
import Data.Text (pack, Text)
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server
import System.Environment
import Twilio (runTwilio')
import Twilio.Messages (post, PostMessage(..))

import SMS

type SMSServerAPI =
  "api" :> "sms" :> ReqBody '[FormUrlEncoded] IncomingMessage :> Post '[JSON] () :<|>
  "api" :> "ping" :> Get '[JSON] String

pingHandler :: Handler String
pingHandler = return "Pong"

incomingHandler :: IncomingMessage -> Handler ()
incomingHandler (IncomingMessage from body) = liftIO $ do
  twilioNum <- fetchTwilioNumber
  runTwilio' fetchSid fetchToken $ do
    let newMessage = PostMessage from twilioNum body Nothing
    _ <- post newMessage
    return ()

smsServerAPI :: Proxy SMSServerAPI
smsServerAPI = Proxy :: Proxy SMSServerAPI

smsServer :: Server SMSServerAPI
smsServer = incomingHandler :<|> pingHandler

runServer :: IO ()
runServer = do
  port <- read <$> getEnv "PORT"
  run port (serve smsServerAPI smsServer)
