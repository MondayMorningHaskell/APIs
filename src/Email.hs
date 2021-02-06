{-# LANGUAGE OverloadedStrings #-}

module Email where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Mail.Hailgun
import System.Directory
import System.Environment

-- Sending Email

sendBasicEmail :: IO ()
sendBasicEmail = do
  domain <- getEnv "MAILGUN_DOMAIN"
  apiKey <- getEnv "MAILGUN_API_KEY"
  replyAddress <- pack <$> getEnv "MAILGUN_REPLY_ADDRESS"
  toAddress <- pack <$> getEnv "MAILGUN_USER_ADDRESS"
  let context = HailgunContext domain apiKey Nothing
  case mkMessage toAddress replyAddress of
    Left err -> putStrLn ("Making failed: " ++ show err)
    Right msg -> do
      result <- sendEmail context msg
      case result of
        Left err -> putStrLn ("Sending failed: " ++ show err)
        Right resp -> putStrLn ("Sending succeeded: " ++ show resp)
  where
    mkMessage toAddress replyAddress = hailgunMessage
      "Hello Mailgun!"
      (TextOnly "This is a test message being sent from our mailgun server.")
      replyAddress 
      (emptyMessageRecipients { recipientsTo = [toAddress] })
      []

sendSubscribeEmail :: Text -> IO ()
sendSubscribeEmail email = do
  domain <- getEnv "MAILGUN_DOMAIN"
  apiKey <- getEnv "MAILGUN_API_KEY"
  replyAddress <- pack <$> getEnv "MAILGUN_REPLY_ADDRESS"
  let context = HailgunContext domain apiKey Nothing
  currentDir <- getCurrentDirectory
  case mkSubscribeMessage replyAddress (encodeUtf8 email) currentDir of
    Left err -> putStrLn ("Making failed: " ++ show err)
    Right msg -> do
      result <- sendEmail context msg
      case result of
        Left err -> putStrLn ("Sending failed: " ++ show err)
        Right resp -> putStrLn ("Sending succeeded: " ++ show resp)

mkSubscribeMessage :: ByteString -> ByteString -> FilePath -> Either HailgunErrorMessage HailgunMessage
mkSubscribeMessage replyAddress subscriberAddress currentDir = hailgunMessage
  "Thanks for signing up!"
  content
  replyAddress 
  (emptyMessageRecipients { recipientsTo = [subscriberAddress] })
  [Attachment (rewardFilepath currentDir) (AttachmentBS "Your Reward")]
  where
    content = TextAndHTML 
      textOnly
      ("Here's your reward! To confirm your subscription, click " <> link <> "!")
    textOnly = "Here's your reward! To confirm your subscription, go to "
      <> "https://mmh-apis.herokuapp.com/api/subscribe/" <> subscriberAddress
      <> " and we'll sign you up!"
    link = "<a href=\"https://mmh-apis.herokuapp.com/api/subscribe/"
      <> subscriberAddress <> "\">this link</a>"

rewardFilepath :: FilePath -> FilePath
rewardFilepath currentDir = currentDir ++ "/attachments/reward.txt"

mkListMessage :: ByteString -> (Text, ByteString, Maybe ByteString) -> [Text] -> Either HailgunErrorMessage HailgunMessage
mkListMessage replyAddress (subject, txtOnly, maybeHTML) subscribers = hailgunMessage
  subject
  finalContent
  replyAddress
  (emptyMessageRecipients { recipientsBCC = map encodeUtf8 subscribers })
  []
  where
    finalContent = case maybeHTML of
      Nothing -> TextOnly txtOnly
      Just html -> TextAndHTML txtOnly html
