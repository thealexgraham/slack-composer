{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module SlackComposer.Slack where

import           SlackComposer.Amper
import           SlackComposer.Slack.API
import           SlackComposer.Amper.SimpleRender

import           Control.Monad.Except

import           Data.Aeson hiding ((<?>))
import           Data.Monoid
import qualified Data.Text as T
import           Database.PostgreSQL.Simple

import           Network.HTTP.Client.TLS (newTlsManager)

import           Servant
import           Servant.Client

-- |Receives a 'compose' message from Slack and sends a request to the Amper API
-- to compose a piece of music. On completion of render, sends a message to the
-- channel with a link to the download.
slackServer
    :: T.Text            -- ^API Key
    -> Maybe Connection  -- ^PostgresSQL connection
    -> Server SlackAPI
slackServer key conn = amperCompose
             :<|> return "Nothing here but us roots!"
    where
        amperCompose :: SlackPayload -> Handler T.Text
        amperCompose pl = do
            liftIO $ doRenderRequest key txt updateResponder finishedResponder
            return $ "Thanks! Composing some *" <> txt <> "*..."
            where
                txt = spText pl
                url = spResponseUrl pl
                usr = spUserName pl
                uid = spUserId pl

                finishedResponder :: SimpleRenderResponse -> IO ()
                finishedResponder rsp = do
                    maybe (return ())
                          (insertNewComposition (T.pack uid) usr txt (T.pack $ show $ encode tl)) conn

                    case durl of
                        (Just x) -> sendSlackMessage slackMessageChannel url (um <> (msg x))
                                 >> sendSlackMessage slackMessageUser url tlMsg
                        Nothing  -> updateResponder "Render error"
                    where
                        tlr     = attrTimelineResponse $ dataAttributes $ srData rsp
                        tl      = attrTimeline $ dataAttributes $ srData rsp
                        um      = "<@" <> usr <> "> "
                        durl    = getMP3DownloadURL rsp (txt <> ".mp3")
                        msg lnk = "composed some *" <> txt <> "*! Check it out "
                               <> ("<" <> lnk <> "|by clicking this link." <> ">")
                        tlMsg   = formatRegions tlr

                updateResponder :: T.Text -> IO ()
                updateResponder msg = sendSlackMessage slackMessageUser url msg

-- |Format the names of descriptors of a 'TimelineResponse' into a readable message
-- for informing the user.
formatRegions :: Maybe TimelineResponse -> T.Text
formatRegions Nothing   = "No regions detected."
formatRegions (Just tr) = T.intercalate " \n " msgs
    where
        rs   = concatMap srRegions $ trSpans tr
        msgs = map (\(RegionInfo b d) -> "beat " <> (T.pack $ show b) <> ": " <> d) rs

slackHost :: T.Text
slackHost = "hooks.slack.com"

sendSlackMessage
    :: (T.Text -> ClientM NoContent)
    -> T.Text -- ^ url
    -> T.Text -- ^ message
    -> IO ()
sendSlackMessage messager url msg = do
    env <- flip mkClientEnv (BaseUrl Https (T.unpack slackHost) 443 (T.unpack url')) <$> newTlsManager
    res <- runClientM (messager msg) env
    case res of
        (Left err) -> print err
        (Right x)  -> print x
    return ()
    where
        url' = T.replace "https://hooks.slack.com/" "" url

-- |Insert a composition request and response into the database, if using.
insertNewComposition :: T.Text -> T.Text -> T.Text -> T.Text -> Connection -> IO ()
insertNewComposition i nm q tl conn
    = void $ execute conn ("INSERT INTO compositions "
                       <> "(user_id,user_name,query,timeline)"
                       <> "VALUES (?,?,?,?)")
             (i,nm,q,tl)

-- |Query to create a table for storing composition requests in
makeTableQuery :: Query
makeTableQuery = "CREATE TABLE IF NOT EXISTS "
              <> "compositions"
              <> "(id SERIAL PRIMARY KEY, "
              <>  "user_id   text, "
              <>  "user_name text, "
              <>  "query     text, "
              <>  "timeline  text, "
              <>  "created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP)"

slackApp
    :: T.Text           -- ^API key
    -> Maybe Connection -- ^optional postgres connection
    -> Application
slackApp key conn = serve slackAPI (slackServer key conn)
