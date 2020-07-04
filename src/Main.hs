{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Database.PostgreSQL.Simple
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import           Data.Monoid
import           System.Environment (getArgs)
import           Text.Read
import           Network.Wai.Handler.Warp (run)

import           SlackComposer.Slack
import           SlackComposer.CommandLineOptions

main :: IO ()
main = do
    clo <- getArgs >>= parseCommandLineOptions

    let prt = maybe (error "--port argument must exist and be a valid number")
                    id (handlePort clo)

        key = maybe (error "--api-key argument must exist")
                    id (clAPIKey clo)

    conn <- handleConnection $ clDatabaseURI clo

    print $ "running server on " <> show prt
    run prt (slackApp (T.pack key) conn)

-- |Extract the port argument and read it into an 'Int'
handlePort :: CommandLineOptions -> Maybe Int
handlePort clo = clPort clo >>= readMaybe

-- |Extract the Postgres database argument. If it exists, create
-- the necessary tables.
handleConnection :: Maybe String -> IO (Maybe Connection)
handleConnection Nothing    = return Nothing
handleConnection (Just uri) = do
    conn <- connectPostgreSQL (BC.pack uri)
    _    <- execute_ conn makeTableQuery

    return $ Just conn

