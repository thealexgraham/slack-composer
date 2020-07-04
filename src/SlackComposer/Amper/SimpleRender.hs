{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module SlackComposer.Amper.SimpleRender where

import           Data.Aeson
import           Data.Aeson.Types

import           GHC.Generics
import qualified Data.Text as T
import           Data.String

data Timeline = Timeline {
         tlRandomSeed :: Maybe Int
        ,tlSpans      :: [Span]
        ,tlInfo       :: Maybe TimelineInfo
    } deriving (Generic)

instance FromJSON Timeline where
    parseJSON = withObject "timeline" $ \o ->
        Timeline <$> o .: "random_seed"
                 <*> o .: "spans"
                 <*> o .: "info"

instance ToJSON Timeline where
    toJSON a = object [ "random_seed" .= tlRandomSeed a
                      , "spans"       .= tlSpans a
                      , "info"        .= tlInfo a ]

data Span = Span {
         sId      :: Int
        ,sTime    :: Int
        ,sType    :: String
        ,sActions :: Maybe [Action]
    } deriving (Generic)

instance FromJSON Span where
    parseJSON = withObject "span" $ \o ->
        Span <$> o .: "id"
             <*> o .: "time"
             <*> o .: "type"
             <*> o .: "actions"

instance ToJSON Span where
    toJSON a = object [ "id"      .= sId a
                      , "time"    .= sTime a
                      , "type"    .= sType a
                      , "actions" .= sActions a ]

data Action = Action {
         create :: [CreateUsing]
    } deriving (Show,Eq,Generic)

instance FromJSON Action
instance ToJSON Action

data CreateUsing = CreateUsing {
         using :: String
    } deriving (Show,Eq,Generic)

instance FromJSON CreateUsing
instance ToJSON CreateUsing

data TimelineInfo = TimelineInfo {
         created_actions :: Maybe Value
    } deriving (Show,Eq,Generic)

instance FromJSON TimelineInfo
instance ToJSON   TimelineInfo

createActionTimeline :: Int -> String -> Timeline
createActionTimeline n str
    = Timeline Nothing
               [ Span 0 0 "metered" (Just $ [Action [ CreateUsing str ]])
               , Span 1 n "unmetered" Nothing ]
               (Just $ TimelineInfo Nothing)

data SimpleRenderAttributes = SimpleRenderAttributes {
         saTimeline           :: Timeline
        ,saMakeTimeline       :: Bool
        ,saPreset             :: String
        ,saAttachmentFilename :: String
    } deriving (Generic)

instance FromJSON SimpleRenderAttributes where
    parseJSON = withObject "simpleRender_attributes" $ \o ->
        SimpleRenderAttributes <$> o .: "timeline"
                               <*> o .: "make_timeline"
                               <*> o .: "preset"
                               <*> o .: "attachment_filename"

instance ToJSON SimpleRenderAttributes where
    toJSON a = object [ "timeline"            .= saTimeline a
                      , "make_timeline"       .= saMakeTimeline a
                      , "preset"              .= saPreset a
                      , "attachment_filename" .= saAttachmentFilename a ]

data SimpleRenderRequest = SimpleRenderRequest {
         rType       :: String
        ,rAttributes :: SimpleRenderAttributes
    } deriving (Generic)

instance ToJSON SimpleRenderRequest where
    toJSON a = object [ "data" .= object [ "type"       .= String "simple_renders"
                                         , "attributes" .= rAttributes a ] ]

instance FromJSON SimpleRenderRequest where
    parseJSON = withObject "simple_render_request" $ \o ->
            SimpleRenderRequest <$> o.: "type"
                                <*> o .: "attributes"

createSimpleRenderRequest :: String -> Int -> T.Text -> SimpleRenderRequest
createSimpleRenderRequest fn n str
    = SimpleRenderRequest "simple_renders" ra
    where
        ra = SimpleRenderAttributes (createActionTimeline n (T.unpack str))
                                    True
                                    "MASTER_MP3"
                                    fn

data RenderStatus = WaitingCompose
                  | RunningCompose
                  | WaitingRender
                  | RunningRender
                  | Created
                  | FailedCreate
    deriving (Show,Eq)

instance ToJSON RenderStatus where
    toJSON WaitingCompose = "WAITING_COMPOSE"
    toJSON RunningCompose = "RUNNING_COMPOSE"
    toJSON WaitingRender  = "WAITING_RENDER"
    toJSON RunningRender  = "RUNNING_RENDER"
    toJSON Created        = "CREATED"
    toJSON FailedCreate   = "FAILED_CREATE"

instance FromJSON RenderStatus where
    parseJSON = withText "render_status" $ \case
        "WAITING_COMPOSE" -> return WaitingCompose
        "RUNNING_COMPOSE" -> return RunningCompose
        "WAITING_RENDER"  -> return WaitingRender
        "RUNNING_RENDER"  -> return RunningRender
        "CREATED"         -> return Created
        "FAILED_CREATE"   -> return FailedCreate
        _                 -> fail "Could not parse render_status"

data RequestFile = RequestFile {
        fFilename    :: Maybe T.Text
       ,fContentType :: T.Text
       ,fURL         :: T.Text
    } deriving (Show,Eq)

instance FromJSON RequestFile where
    parseJSON = withObject "request_file" $ \o ->
        RequestFile <$> o .: "attachment_filename"
                    <*> o .: "content_type"
                    <*> o .: "download_url"

instance ToJSON RequestFile where
    toJSON (RequestFile fn ct u) = object [ "attachment_filename" .= fn
                                          , "content_type"        .= ct
                                          , "download_url"        .= u ]

data Attributes = Attributes {
         attrStatus           :: RenderStatus
        ,attrProgressPercent  :: (Maybe Value)
        ,attrDateCreate       :: T.Text
        ,attrDateUpdate       :: Maybe T.Text
        ,attrFiles            :: [(Maybe RequestFile)]
        ,attrTimeline         :: (Maybe Value)
        ,attrTimelineResponse :: (Maybe TimelineResponse)
  } deriving (Show,Eq,Generic)

instance FromJSON Attributes where
    parseJSON = withObject "" $ \o ->
        Attributes <$> o .: "status"
                   <*> o .:? "progress_percent"
                   <*> o .:  "date_create"
                   <*> o .:  "date_update"
                   <*> o .:  "files"
                   <*> o .:? "timeline"
                   <*> o .:? "timeline"

instance ToJSON Attributes where
  toJSON (Attributes {..}) = object [ "status"           .= attrStatus
                                    , "progress_percent" .= attrProgressPercent
                                    , "date_create"      .= attrDateCreate
                                    , "date_update"      .= attrDateUpdate
                                    , "files"            .= attrFiles
                                    , "timeline"         .= attrTimeline ]

data TimelineResponse = TimelineResponse {
         trSpans :: [SpanResponse]
    } deriving (Show,Eq)

instance FromJSON TimelineResponse where
    parseJSON = withObject "timeline_response" $ \o ->
        TimelineResponse <$> o .: "spans"

data SpanResponse = SpanResponse {
        srRegions :: [RegionInfo]
    } deriving (Show,Eq)

instance FromJSON SpanResponse where
    parseJSON = withObject "span_response" $ \o -> do
        typ <- o .: "type" :: (Parser String)
        case typ of
            "metered" -> SpanResponse <$> o .: "regions"
            _         -> SpanResponse <$> parseJSON emptyArray

data RegionInfo = RegionInfo {
         riBeat       :: Int
        ,riDescriptor :: T.Text
    } deriving (Show,Eq)

instance FromJSON RegionInfo where
    parseJSON = withObject "region_info" $ \o ->
        RegionInfo <$> o .: "beat"
                   <*> o .: "descriptor"

data Links = Links {
    linksSelf :: T.Text
  } deriving (Show,Eq,Generic)


instance FromJSON Links where
    parseJSON = withObject "" $ \o ->
        Links <$> o .:  "self"

instance ToJSON Links where
  toJSON     (Links {..}) = object ["self" .= linksSelf]

newtype RenderID = RenderID { unRenderID :: T.Text }
    deriving (Show,Eq,IsString,FromJSON,ToJSON)

data Data = Data {
         dataAttributes :: Attributes
        ,dataId         :: RenderID
        ,dataType       :: T.Text
        ,dataLinks      :: Links
  } deriving (Show,Eq,Generic)

instance FromJSON Data where
    parseJSON = withObject "render_data" $ \o ->
                Data <$> o .:  "attributes"
                     <*> o .:  "id"
                     <*> o .:  "type"
                     <*> o .:  "links"

instance ToJSON Data where
    toJSON (Data {..}) = object [ "attributes" .= dataAttributes
                                , "id"         .= dataId
                                , "type"       .= dataType
                                , "links"      .= dataLinks ]

data SimpleRenderResponse = SimpleRenderResponse {
         srData     :: Data
        ,srResponse :: Links
  } deriving (Show,Eq,Generic)


instance FromJSON SimpleRenderResponse where
  parseJSON = withObject "" $ \o ->
        SimpleRenderResponse <$> o .: "data"
                             <*> o .: "links"

instance ToJSON SimpleRenderResponse where
  toJSON (SimpleRenderResponse {..}) = object [ "data"  .= srData
                                              , "links" .= srResponse ]


data SimpleRenderResponseB = SR {
         srID               :: RenderID
        ,srStatus           :: RenderStatus
        ,srFiles            :: [(Maybe RequestFile)]
        ,srTimeline         :: Maybe Value
        ,srTimelineResponse :: Maybe TimelineResponse
    } deriving (Show,Eq)

instance FromJSON SimpleRenderResponseB where
    parseJSON = withObject "simple_response_b" $ \o -> do
        d  <- o .: "data"
        i  <- d .: "id"
        as <- d .: "attributes"

        st   <- as .:  "status"
        fls  <- as .:  "files"
        tl   <- as .:? "timeline"
        tlr  <- as .:? "timeline"

        return $ SR i st fls tl tlr
