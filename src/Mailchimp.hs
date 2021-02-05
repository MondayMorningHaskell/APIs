{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Mailchimp where

import Control.Monad (forM)
import Data.Aeson
import Data.ByteString.Char8 (pack)
import Data.Int (Int64)
import Data.List (find)
import Data.Proxy
import Data.Text (Text)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.API
import Servant.API.BasicAuth (BasicAuthData)
import Servant.Client
import Servant.Server
import System.Environment

type MCAuth = BasicAuth "mailchimp" ()

type MailchimpApi = 
  MCAuth :> "lists" :> Get '[JSON] MailchimpListResponse :<|>
  MCAuth :> "lists" :> Capture "list-id" Text :> "members" :> QueryParam "count" Int :> Get '[JSON] MailchimpMembersResponse :<|>
  MCAuth :> "lists" :> Capture "list-id" Text :> "members" :> ReqBody '[JSON] MailchimpSubscriber :> Post '[JSON] ()

mailchimpApi :: Proxy MailchimpApi
mailchimpApi = Proxy :: Proxy MailchimpApi

fetchListsClient :: BasicAuthData -> ClientM MailchimpListResponse
fetchSubscribersClient :: BasicAuthData -> Text -> Maybe Int -> ClientM MailchimpMembersResponse
subscribeNewUserClient :: BasicAuthData -> Text -> MailchimpSubscriber -> ClientM ()
( fetchListsClient :<|>
  fetchSubscribersClient :<|>
  subscribeNewUserClient) = client mailchimpApi

--
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

fetchMCListId :: Text -> IO (Either String Text)
fetchMCListId listName = do
  listsResponse <- runMailchimp fetchListsClient
  case listsResponse of
    Left err -> return $ Left (show err)
    Right (MailchimpListResponse lists) -> case find nameMatches lists of
      Nothing -> return $ Left "Couldn't find list with that name!"
      Just (MailchimpSingleList (_, id_)) -> return $ Right id_ 
  where
    nameMatches :: MailchimpSingleList -> Bool
    nameMatches (MailchimpSingleList (name, id_)) = name == listName

fetchMCListMembers :: Text -> IO (Either String [Text])
fetchMCListMembers listId = do
  membersResponse <- runMailchimp (\auth -> fetchSubscribersClient auth listId (Just 2000))
  case membersResponse of
    Left err -> return $ Left (show err)
    Right (MailchimpMembersResponse subs) -> return $ Right (map unMailchimpSubscriber subs)

subscribeMCMember :: Text -> Text -> IO (Either String ())
subscribeMCMember listId email = do
  subscribeResponse <- runMailchimp (\auth -> subscribeNewUserClient auth listId (MailchimpSubscriber email))
  case subscribeResponse of
    Left err -> return $ Left (show err)
    Right _ -> return $ Right ()

runMailchimp :: (BasicAuthData -> ClientM a) -> IO (Either ServantError a)
runMailchimp action = do
  baseUrl <- getEnv "MAILCHIMP_BASE_URL"
  apiKey <- getEnv "MAILCHIMP_API_KEY"
  trueUrl <- parseBaseUrl baseUrl
  let userData = BasicAuthData "username" (pack apiKey)
  manager <- newTlsManager
  let clientEnv = ClientEnv manager trueUrl Nothing
  runClientM (action userData) clientEnv

getListId :: IO ()
getListId = do
  baseUrl <- getEnv "MAILCHIMP_BASE_URL"
  apiKey <- getEnv "MAILCHIMP_API_KEY"
  trueUrl <- parseBaseUrl baseUrl
  let userData = BasicAuthData "username" (pack apiKey)
  manager <- newTlsManager
  let clientEnv = ClientEnv manager trueUrl Nothing
  lists <- runClientM (fetchListsClient userData) clientEnv
  print "----- LISTS -----"
  print lists
  subs <- runClientM (fetchSubscribersClient userData "1e1d808e31" (Just 1000)) clientEnv
  print "----- SUBSCRIBERS -----"
  print subs
  -- resp <- runClientM (subscribeNewUserClient userData "1e1d808e31" (MailchimpSubscriber "jbowen@g.hmc.edu")) clientEnv
  -- print "----- SUBSCRIBE RESPONSE -----"
  -- print resp


