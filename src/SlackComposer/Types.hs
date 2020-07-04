{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SlackComposer.Types where

import           Data.Aeson hiding ((<?>))
import qualified Data.Aeson.Parser
import           Data.Aeson.Types (parseEither)
import           Data.Attoparsec.ByteString.Char8
                    (endOfInput, parseOnly, skipSpace, (<?>))
import           Data.Monoid
import           Network.HTTP.Media ((//))
import           Servant

import qualified Data.ByteString.Lazy as BS
import           Data.String.Conversions (cs)
import qualified Data.Text as T


data VendorJSON

-- | @application/json@
instance Accept VendorJSON where
    contentTypes _ =
      [ "application" // "vnd.api+json" ]
instance FromJSON a => MimeUnrender VendorJSON a where
    mimeUnrender _ = eitherDecodeLenient

eitherDecodeLenient :: FromJSON a => BS.ByteString -> Either String a
eitherDecodeLenient input =
    parseOnly parser (cs input) >>= parseEither parseJSON
  where
    parser = skipSpace
          *> Data.Aeson.Parser.value
          <* skipSpace
          <* (endOfInput <?> "trailing junk after valid JSON")

data Authorization = Bearer T.Text
    deriving (Show,Eq)

instance ToHttpApiData Authorization where
    toUrlPiece (Bearer t) = "Bearer " <> t

newtype Second = Second { unSecond :: Int } deriving (Num)

fromSeconds :: Double -> Int
fromSeconds n = floor (n * 1e+06 :: Double)
