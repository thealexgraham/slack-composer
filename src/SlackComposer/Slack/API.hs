{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module SlackComposer.Slack.API where

import           Data.Aeson hiding ((<?>))
import qualified Data.Text as T
import           GHC.Generics (Generic)

import           Web.FormUrlEncoded

import           Servant
import           Servant.Client

type SlackAPI = "compose" :> ReqBody '[FormUrlEncoded] SlackPayload
                          :> Post '[PlainText] T.Text
           :<|> Get '[PlainText] T.Text

type SlackClientAPI = ReqBody '[JSON] SlackMessagePayload :> PostNoContent

slackClientAPI :: Proxy SlackClientAPI
slackClientAPI = Proxy

slackMessage :: SlackMessagePayload -> ClientM NoContent
slackMessage = client slackClientAPI

slackMessageUser :: T.Text -> ClientM NoContent
slackMessageUser txt = slackMessage (SlackMessagePayload txt [] Nothing)

slackMessageChannel :: T.Text -> ClientM NoContent
slackMessageChannel txt = slackMessage (SlackMessagePayload txt [] (Just "in_channel"))

data SlackMessagePayload = SlackMessagePayload {
        msgText        :: T.Text
       ,msgBlocks      :: [T.Text]
       ,msgReponseType :: Maybe T.Text
    } deriving (Show,Eq,Generic)

instance ToJSON SlackMessagePayload where
    toJSON x = object [ "text"          .= msgText x
                      , "blocks"        .= msgBlocks x
                      , "response_type" .= msgReponseType x ]

data SlackPayload = SlackPayload {
         spToken        :: String
        ,spUserId      :: String
        ,spUserName    :: T.Text
        ,spCommand      :: T.Text
        ,spText         :: T.Text
        ,spResponseUrl :: T.Text
        -- ,team_id :: String
        -- ,team_domain :: String
        -- ,enterprise_id :: String
        -- ,enterprise_name :: String
        -- ,channel_id :: String
        -- ,trigger_id :: String
    } deriving (Show,Eq,Generic)

instance FromJSON SlackPayload where
    parseJSON = withObject "slack_payload" $ \o ->
        SlackPayload <$> o .: "token"
                     <*> o .: "user_id"
                     <*> o .: "user_name"
                     <*> o .: "command"
                     <*> o .: "text"
                     <*> o .: "response_url"

instance ToJSON SlackPayload where
    toJSON (SlackPayload {..}) = object [ "token"        .= spToken
                                        , "user_id"      .= spUserId
                                        , "user_name"    .= spUserName
                                        , "command"      .= spCommand
                                        , "text"         .= spText
                                        , "response_url" .= spResponseUrl ]

instance FromForm SlackPayload where
  fromForm f = SlackPayload
    <$> parseUnique "token"        f
    <*> parseUnique "user_id"      f
    <*> parseUnique "user_name"    f
    <*> parseUnique "command"      f
    <*> parseUnique "text"         f
    <*> parseUnique "response_url" f

slackAPI :: Proxy SlackAPI
slackAPI = Proxy

