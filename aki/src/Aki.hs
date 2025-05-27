{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Aki where

import           Data.Aeson       (FromJSON (..), ToJSON (..))
import qualified Data.Aeson       as Aeson
import qualified Data.Char        as C
import qualified Data.List        as L
import           Data.Maybe       (fromMaybe)
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Word
import           GHC.Generics     (Generic)
import qualified Network.HTTP.Req as Req

ankiConnectVersion :: Int
ankiConnectVersion = 6

ankiConnectPort :: Int
ankiConnectPort = 18765

akiDeck :: Text
akiDeck = "daily"

--------------------------------------------------------------------------------
-- AnkiConnect Req/Resp
--------------------------------------------------------------------------------

standardAesonGenericOptions :: [(String, String)] -> Aeson.Options
standardAesonGenericOptions fieldLabels =
  Aeson.defaultOptions
  { Aeson.omitNothingFields  = True
  , Aeson.fieldLabelModifier = \field ->
      fromMaybe ("JSON field missing: " <> show field) (L.lookup field fieldLabels)
  }

data AkiRequest a = AkiRequest
  { akiRequestAction  :: !Text
  , akiRequestVersion :: !Int
  , akiRequestParams  :: !a
  , akiRequestAuthKey :: !(Maybe Text)
  } deriving (Show, Eq, Generic)
akiRequestAesonOptions :: Aeson.Options
akiRequestAesonOptions = standardAesonGenericOptions
  [ ("akiRequestAction", "action")
  , ("akiRequestVersion", "version")
  , ("akiRequestParams", "params")
  , ("akiRequestAuthKey", "authKey")
  ]
instance FromJSON a => FromJSON (AkiRequest a) where
  parseJSON = Aeson.genericParseJSON akiRequestAesonOptions
instance ToJSON a => ToJSON (AkiRequest a) where
  toJSON = Aeson.genericToJSON akiRequestAesonOptions

data AkiResponse a = AkiResponse
  { akiResponseError  :: !(Maybe Text)
  , akiResponseResult :: !a
  } deriving (Show, Eq, Generic)
akiResponseAesonOptions :: Aeson.Options
akiResponseAesonOptions = standardAesonGenericOptions
  [ ("akiResponseError", "error")
  , ("akiResponseResult", "result")
  ]
instance FromJSON a => FromJSON (AkiResponse a) where
  parseJSON = Aeson.genericParseJSON akiResponseAesonOptions
instance ToJSON a => ToJSON (AkiResponse a) where
  toJSON = Aeson.genericToJSON akiResponseAesonOptions

--------------------------------------------------------------------------------
-- AnkiConnect addNote API
--------------------------------------------------------------------------------

data BasicNoteFields = BasicNoteFields
  { basicNoteFront :: !Text
  , basicNoteBack  :: !Text
  } deriving (Show, Eq, Generic)
basicNoteFieldsAesonOptions :: Aeson.Options
basicNoteFieldsAesonOptions = standardAesonGenericOptions
  [ ("basicNoteFront", "Front")
  , ("basicNoteBack", "Back")
  ]
instance FromJSON BasicNoteFields where
  parseJSON = Aeson.genericParseJSON basicNoteFieldsAesonOptions
instance ToJSON BasicNoteFields where
  toJSON = Aeson.genericToJSON basicNoteFieldsAesonOptions

data NoteDupScopeOptions = NoteDupScopeOptions
  { dupDeckName       :: !Text
  , dupCheckChildren  :: !Bool
  , dupCheckAllModels :: !Bool
  } deriving (Show, Eq, Generic)
noteDupScopeOptionsAesonOptions :: Aeson.Options
noteDupScopeOptionsAesonOptions = standardAesonGenericOptions
  [ ("dupDeckName", "deckName")
  , ("dupCheckChildren", "checkChildren")
  , ("dupCheckAllModels", "checkAllModels")
  ]
instance FromJSON NoteDupScopeOptions where
  parseJSON = Aeson.genericParseJSON noteDupScopeOptionsAesonOptions
instance ToJSON NoteDupScopeOptions where
  toJSON = Aeson.genericToJSON noteDupScopeOptionsAesonOptions

data NoteOptions = NoteOptions
  { noteOptionsAllowDuplicate  :: !Bool
  , noteOptionsDuplicateScope  :: !Text -- "deck" or any other text
  , noteOptionsDupScopeOptions :: !(Maybe NoteDupScopeOptions)
  } deriving (Show, Eq, Generic)
noteOptionsAesonOptions :: Aeson.Options
noteOptionsAesonOptions = standardAesonGenericOptions
  [ ("noteOptionsAllowDuplicate", "allowDuplicate")
  , ("noteOptionsDuplicateScope", "duplicateScope")
  , ("noteOptionsDupScopeOptions", "duplicateScopeOptions")
  ]
instance FromJSON NoteOptions where
  parseJSON = Aeson.genericParseJSON noteOptionsAesonOptions
instance ToJSON NoteOptions where
  toJSON = Aeson.genericToJSON noteOptionsAesonOptions

allowDuplicateOptions :: NoteOptions
allowDuplicateOptions = NoteOptions
  { noteOptionsAllowDuplicate  = True
  , noteOptionsDuplicateScope  = "deck"
  , noteOptionsDupScopeOptions = Just $ NoteDupScopeOptions
      { dupDeckName       = akiDeck
      , dupCheckChildren  = True
      , dupCheckAllModels = False
      }
  }

data Note = Note
  { noteDeckName  :: !Text
  , noteModelName :: !Text
  , noteFields    :: !BasicNoteFields
  , noteTags      :: ![Text]
  , noteOptions   :: !(Maybe NoteOptions)
  } deriving (Show, Eq, Generic)
noteAesonOptions :: Aeson.Options
noteAesonOptions =
  options { Aeson.sumEncoding            = Aeson.ObjectWithSingleField
          , Aeson.constructorTagModifier = fmap C.toLower
          , Aeson.tagSingleConstructors  = True
          }
  where
    options = standardAesonGenericOptions
              [ ("noteDeckName", "deckName")
              , ("noteModelName", "modelName")
              , ("noteFields", "fields")
              , ("noteTags", "tags")
              , ("noteOptions", "options")
              ]
instance FromJSON Note where
  parseJSON = Aeson.genericParseJSON noteAesonOptions
instance ToJSON Note where
  toJSON = Aeson.genericToJSON noteAesonOptions


type AddNoteRequest  = AkiRequest  Note
type AddNoteResponse = AkiResponse (Maybe Word64)

mkBasicNote :: Text -> Text -> AddNoteRequest
mkBasicNote front back = AkiRequest
  { akiRequestAction  = "addNote"
  , akiRequestVersion = ankiConnectVersion
  , akiRequestParams  = Note
      { noteDeckName  = akiDeck
      , noteModelName = "Basic"
      , noteFields    = BasicNoteFields front back
      , noteTags      = ["aki"]
      , noteOptions   = Nothing
      }
  , akiRequestAuthKey = Nothing
  }

addBasicNote :: Text -> Text -> IO (Either String Word64)
addBasicNote front back = do
  let akiReq  = mkBasicNote front back
      httpReq = Req.req Req.POST
                        (Req.http "127.1")
                        (Req.ReqBodyJson akiReq)
                        (Req.jsonResponse @AddNoteResponse)
                        (Req.port ankiConnectPort)
  response <- Req.responseBody <$> Req.runReq Req.defaultHttpConfig httpReq
  case akiResponseError response of
    Just err -> return . Left  $ "Error adding note: " <> T.unpack err
    Nothing  -> case akiResponseResult response of
      Nothing -> return . Left $ "Unexpected null result"
      Just i  -> return . Right $ i
