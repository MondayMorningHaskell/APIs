{-# LANGUAGE OverloadedStrings #-}

module SMS where

import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text, splitOn, pack)
import System.Environment (getEnv)
import Twilio hiding (runTwilio)
import Twilio.Messages
import Web.FormUrlEncoded (FromForm(..), Form(..))

-- Get Environment Variables

fetchSid :: IO String
fetchSid = (getEnv "TWILIO_ACCOUNT_SID")

fetchToken :: IO String
fetchToken = (getEnv "TWILIO_AUTH_TOKEN")

fetchTwilioNumber :: IO Text
fetchTwilioNumber = pack <$> getEnv "TWILIO_PHONE_NUMBER"

-- SMS Command

data SMSCommand = SubscribeCommand Text

messageToCommand :: Text -> Maybe SMSCommand
messageToCommand messageBody = case splitOn " " messageBody of
  ["subscribe", email] -> Just $ SubscribeCommand email
  _ -> Nothing

-- Sending a Basic Message

sendMessage :: Text -> Text -> IO ()
sendMessage toNumber message = runTwilio' fetchSid fetchToken $ do
  let msg = PostMessage "+15551231234" toNumber message Nothing
  _ <- post msg
  return ()

-- Message Data Type

data IncomingMessage = IncomingMessage
  { fromNumber :: Text
  , body :: Text
  }
  deriving (Show)

instance FromForm IncomingMessage where
  fromForm (Form form) = case lookupResults of
    Just ((fromNumber : _), (body : _)) -> Right $ IncomingMessage fromNumber body
    Just _ -> Left "Found the keys but no values"
    _ -> Left "Didn't find keys" 
    where
      lookupResults = do
        fromNumber <- HashMap.lookup "From" form
        body <- HashMap.lookup "Body" form
        return (fromNumber, body)
