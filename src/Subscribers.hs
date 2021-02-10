{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Subscribers where

import Control.Monad (forM)
import Data.Aeson
import Data.ByteString.Char8 (pack)
import Data.List (find)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.API
import Servant.API.BasicAuth (BasicAuthData)
import Servant.Client
import Servant.Server
import System.Environment

---------- Newtypes and JSON deserialization ----------
newtype MailchimpSubscriber = MailchimpSubscriber { unMailchimpSubscriber :: Text }
  deriving (Show)

instance ToJSON MailchimpSubscriber where
  toJSON (MailchimpSubscriber email) = object
    [ "email_address" .= email
    , "status" .= ("subscribed" :: Text)
    ]

instance FromJSON MailchimpSubscriber where
  parseJSON = withObject "MailchimpSubscriber" $ \o -> do
    email <- o .: "email_address" 
    return $ MailchimpSubscriber email

--
newtype MailchimpSingleList = MailchimpSingleList (Text, Text)
  deriving (Show)

instance FromJSON MailchimpSingleList where
  parseJSON = withObject "MailchimpSingleList" $ \o -> do
    name <- o .: "name"
    id_ <- o .: "id"
    return $ MailchimpSingleList (name, id_)

--
newtype MailchimpListResponse = MailchimpListResponse [MailchimpSingleList]
  deriving (Show)

instance FromJSON MailchimpListResponse where
  parseJSON = withObject "MailchimpListResponse" $ \o -> do
    lists <- o .: "lists"
    MailchimpListResponse <$> forM lists parseJSON

--
newtype MailchimpMembersResponse = MailchimpMembersResponse [MailchimpSubscriber]
  deriving (Show)

instance FromJSON MailchimpMembersResponse where
  parseJSON = withObject "MailchimpMembersResponse" $ \o -> do
    members <- o .: "members"
    MailchimpMembersResponse <$> forM members parseJSON

---------- Servant definitions -----------
type MCAuth = BasicAuth "mailchimp" ()

type MailchimpApi =
  MCAuth :> "lists" :> Get '[JSON] MailchimpListResponse :<|>
  MCAuth :> "lists" :> Capture "list-id" Text :> QueryParam "count" Int :> Get '[JSON] MailchimpMembersResponse :<|>
  MCAuth :> "lists" :> Capture "list-id" Text :> ReqBody '[JSON] MailchimpSubscriber :> Post '[JSON] ()

mailchimpApi :: Proxy MailchimpApi
mailchimpApi = Proxy :: Proxy MailchimpApi

fetchListsClient :: BasicAuthData -> ClientM MailchimpListResponse
fetchSubscribersClient :: BasicAuthData -> Text -> Maybe Int -> ClientM MailchimpMembersResponse
subscribeNewUserClient :: BasicAuthData -> Text -> MailchimpSubscriber -> ClientM ()
( fetchListsClient :<|>
  fetchSubscribersClient :<|>
  subscribeNewUserClient) = client mailchimpApi

--------- Running Mailchimp ----------
runMailchimp :: (BasicAuthData -> ClientM a) -> IO (Either ServantError a)
runMailchimp action = do
  baseUrl <- getEnv "MAILCHIMP_BASE_URL"
  apiKey <- getEnv "MAILCHIMP_API_KEY"
  trueUrl <- parseBaseUrl baseUrl
  -- "username" doesn't matter, we only care about API key as "password"
  let userData = BasicAuthData "username" (pack apiKey)
  manager <- newTlsManager
  let clientEnv = ClientEnv manager trueUrl Nothing
  runClientM (action userData) clientEnv

fetchMailchimpListId :: Text -> IO (Either String Text)
fetchMailchimpListId listName = do
  listsResponse <- runMailchimp fetchListsClient
  case listsResponse of
    Left err -> return $ Left (show err)
    Right (MailchimpListResponse lists) ->
      case find nameMatches lists of
        Nothing -> return $ Left "Couldn't find list with that name!"
        Just (MailchimpSingleList (_, id_)) -> return $ Right id_
  where
    nameMatches :: MailchimpSingleList -> Bool
    nameMatches (MailchimpSingleList (name, _)) = name == listName

fetchMailchimpListMembers :: Text -> IO (Either String [Text])
fetchMailchimpListMembers listId = do
  membersResponse <- runMailchimp 
    (\auth -> fetchSubscribersClient auth listId (Just 2000))
  case membersResponse of
    Left err -> return $ Left (show err)
    Right (MailchimpMembersResponse subs) -> return $
      Right (map unMailchimpSubscriber subs)

subscribeMailchimpMember :: Text -> Text -> IO (Either String ())
subscribeMailchimpMember listId email = do
  subscribeResponse <- runMailchimp (\auth ->
    subscribeNewUserClient auth listId (MailchimpSubscriber email))
  case subscribeResponse of
    Left err -> return $ Left (show err)
    Right _ -> return $ Right ()
