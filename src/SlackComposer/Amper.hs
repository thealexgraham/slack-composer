{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SlackComposer.Amper where

import           SlackComposer.Amper.SimpleRender
import           SlackComposer.Types

import           Control.Concurrent
import           Control.Monad

import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T

import           Network.HTTP.Client.TLS (newTlsManager)

import           Servant
import           Servant.Client

type AmperAPI =  "simple_renders" :> Header "Authorization" Authorization
                                  :> Capture "id" T.Text
                                  :> Get '[VendorJSON] SimpleRenderResponse

            :<|> "simple_renders" :> Header "Authorization" Authorization
                                  :> ReqBody '[JSON] SimpleRenderRequest
                                  :> Post '[VendorJSON] SimpleRenderResponse

amperApi :: Proxy AmperAPI
amperApi = Proxy

-- |Sends an ID and receives the current state of the render.
getSimpleRender
    :: Maybe Authorization
    -> T.Text
    -> ClientM SimpleRenderResponse

-- |Requests a render
requestSimpleRender
    :: Maybe Authorization
    -> SimpleRenderRequest
    -> ClientM SimpleRenderResponse

getSimpleRender :<|> requestSimpleRender = client amperApi

-- |Polls API server at given rate until timeout is reached.
pollServer
    :: ClientEnv
    -> Authorization
    -> Second
    -> RenderID
    -> Int  -- ^timeout
    -> Int  -- ^tries
    -> Bool -- ^has started render
    -> (SimpleRenderResponse -> IO ()) -- ^Created
    -> (SimpleRenderResponse -> IO ()) -- ^RunningRender
    -> (SimpleRenderResponse -> IO ()) -- ^FailedCreate
    -> IO ()
pollServer env auth rate rid tn n dr cfn rfn ffn
    = runClientM (getRenderQuery auth rid) env >>= \case
        (Left err)  -> error $ show err
        (Right rsp) -> do
            let st = attrStatus $ dataAttributes $ srData rsp
            print st
            case st of
                RunningRender  -> (when (not dr) $ rfn rsp) >> repoll True
                FailedCreate   -> ffn rsp
                Created        -> cfn rsp
                _              -> repoll dr
    where
        repoll dr' = do
            threadDelay $ unSecond rate
            when (n < tn) $
                pollServer env auth rate rid tn (n + 1) dr' cfn
                                                            rfn
                                                            ffn

-- |Retrieves the MP3 download URL from a 'SimpleRenderResponse' with the
-- given filename if it exists.
getMP3DownloadURL
    :: SimpleRenderResponse
    -> T.Text
    -> Maybe T.Text
getMP3DownloadURL rsp fn
    | null mfs  = Nothing
    | otherwise = Just $ fURL $ head mfs
    where
        fs  = attrFiles $ dataAttributes $ srData $ rsp
        mfs = catMaybes $ filter f fs

        f :: Maybe RequestFile -> Bool
        f (Just (RequestFile (Just nm) "audio/mp3" _)) = nm == fn
        f _                                            = False

-- |Creates a Bearer Authorization
makeAuth :: T.Text -> Authorization
makeAuth = Bearer

amperBaseUrl :: BaseUrl
amperBaseUrl = (BaseUrl Https "api.ampermusic.com" 443 "/v1")

makeAmperClientEnvironment :: IO ClientEnv
makeAmperClientEnvironment = (`mkClientEnv` amperBaseUrl) <$> newTlsManager

requestRenderQuery :: Authorization -> T.Text -> ClientM SimpleRenderResponse
requestRenderQuery auth txt
    = requestSimpleRender (Just auth)
                          (createSimpleRenderRequest ((T.unpack txt) <> ".mp3") 30 txt)

getRenderQuery :: Authorization -> RenderID -> ClientM SimpleRenderResponse
getRenderQuery auth (RenderID rid) = getSimpleRender (Just auth) rid

-- |Send a render request the the amper server and respond accordingly
doRenderRequest
    :: T.Text -- ^API Key
    -> T.Text
    -> (T.Text -> IO ())               -- ^function for updates
    -> (SimpleRenderResponse -> IO ()) -- ^function to run when complete
    -> IO ()
doRenderRequest key txt updateResponse finishedResponse = do
    let auth = makeAuth key
    env <- makeAmperClientEnvironment
    res <- runClientM (requestRenderQuery auth txt) env
    case res of
        Left err -> putStrLn $ "error: " ++ show err
        Right x  -> void $ forkIO
                         $ pollServer env auth 2 (dataId $ srData x) 500 0 False
                                      whenFinished firstOnRender whenFailed
    where
        whenFinished :: SimpleRenderResponse -> IO ()
        whenFinished rsp = finishedResponse rsp

        firstOnRender :: SimpleRenderResponse -> IO ()
        firstOnRender _ = return ()

        whenFailed :: SimpleRenderResponse -> IO ()
        whenFailed _ = updateResponse "Render failed, try again!"
