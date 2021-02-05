{-# LANGUAGE OverloadedStrings #-}

module SMSServer where

import Data.Text (pack, Text)
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

runServer :: IO ()
runServer = do
  sid <- fetchSid
  token <- fetchToken
  putStrLn sid
  putStrLn token
