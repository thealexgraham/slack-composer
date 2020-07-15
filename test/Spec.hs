{-# LANGUAGE OverloadedStrings #-}

module Main where

import           SlackComposer.Slack
import           SlackComposer.Slack.API

import           Test.Hspec

import           Network.HTTP.Client       hiding (Proxy,port)
import qualified Network.Wai.Handler.Warp  as Warp

import           Servant
import           Servant.Client            hiding (manager,baseUrl)

main :: IO ()
main = hspec composeSpec

withSlackApp :: (Warp.Port -> IO ()) -> IO ()
withSlackApp action = Warp.testWithApplication (pure $ slackApp "" Nothing) action

composeSpec :: Spec
composeSpec = around withSlackApp $ do
    let compose :<|> _ = client (Proxy :: Proxy SlackAPI)
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings

    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

    describe "POST /compose" $ do
        it "should respond with a 'composing...' message" $ \prt -> do
            result <- runClientM (compose testSlackPayload) (clientEnv prt)
            result `shouldBe` (Right $ "Thanks! Composing some *example text*...")

testSlackPayload :: SlackPayload
testSlackPayload = SlackPayload "token" "id" "my.name" "command" "example text" "http://localhost/response_url"
