{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module SMSServer where

import Data.Proxy (Proxy(..))
import Data.Text (pack, Text)
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server
import System.Environment
import Twilio (runTwilio')
import Twilio.Messages (post, PostMessage(..))

fetchSid :: IO String
fetchSid = getEnv "TWILIO_ACCOUNT_SID"

fetchToken :: IO String
fetchToken = getEnv "TWILIO_AUTH_TOKEN"

fetchTwilioNumber :: IO Text 
fetchTwilioNumber = pack <$> getEnv "TWILIO_PHONE_NUMBER"

fetchUserNumber :: IO Text
fetchUserNumber = pack <$> getEnv "TWILIO_USER_NUMBER"

sendBasicMessage :: IO ()
sendBasicMessage = do
  toNumber <- fetchUserNumber
  fromNumber <- fetchTwilioNumber
  runTwilio' fetchSid fetchToken $ do
    let msg = PostMessage toNumber fromNumber "Hello Twilio!" Nothing
    _ <- post msg
    return ()

type SMSServerAPI = "api" :> "ping" :> Get '[JSON] String

pingHandler :: Handler String
pingHandler = return "Pong"

smsServerAPI :: Proxy SMSServerAPI
smsServerAPI = Proxy :: Proxy SMSServerAPI

smsServer :: Server SMSServerAPI
smsServer = pingHandler

runServer :: IO ()
runServer = do
  port <- read <$> getEnv "PORT"
  run port (serve smsServerAPI smsServer)
