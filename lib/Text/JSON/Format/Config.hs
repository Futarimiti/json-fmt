{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Text.JSON.Format.Config
  ( ValueType (..), getValueType
  , Config, parseConfigJSON
  , spaceNBeforeColon
  , spaceNAfterColon
  , spaceNBeforeArrComma
  , spaceNAfterArrComma
  , arrPaddingSpaceN
  , spaceNInEmptyArr
  , spaceNInEmptyObj
  , objPaddingSpaceN
  , endWithNewline
  , oneEntryOneLine
  , oneElemOneLine
  , elemsOnSepLine
  ) where

import           Control.Lens           (Lens', makeLenses, set, (^.))
import           Control.Monad.Identity (Identity)
import           Data.Aeson             (FromJSON (..), Options (..),
                                         defaultOptions, eitherDecodeStrict,
                                         genericParseJSON)
import qualified Data.Aeson             as Aeson
import           Data.ByteString        (ByteString)
import           Data.Default           (Default (..))
import           Data.Function
import           Data.Maybe             (fromMaybe)
import           GHC.Generics           (Generic)

data ValueType = Empty
               | Null
               | Bool
               | Number
               | EmptyString
               | NonEmptyString
               | FilledArray
               | EmptyArray
               | FilledObject
               | EmptyObject
               deriving (Show, Eq, Read, Generic, FromJSON)

getValueType :: Aeson.Value -> ValueType
getValueType = \case Aeson.Null -> Null
                     Aeson.Bool _ -> Bool
                     Aeson.Number _ -> Number
                     Aeson.String "" -> EmptyString
                     Aeson.String _ -> NonEmptyString
                     Aeson.Array arr | null arr -> EmptyArray
                                     | otherwise -> FilledArray
                     Aeson.Object obj | null obj -> EmptyObject
                                      | otherwise -> FilledObject

type family Field f a where
  Field Identity a = a
  Field f a = f a

data ConfigOf f = Config { _spaceNBeforeColon    :: Field f Int
                         , _spaceNAfterColon     :: Field f Int
                         , _spaceNBeforeArrComma :: Field f Int
                         , _spaceNAfterArrComma  :: Field f Int
                         , _arrPaddingSpaceN     :: Field f Int
                         , _spaceNInEmptyArr     :: Field f Int
                         , _spaceNInEmptyObj     :: Field f Int
                         , _objPaddingSpaceN     :: Field f Int
                         , _endWithNewline       :: Field f Bool
                         , _oneEntryOneLine      :: Field f [ValueType]
                         , _oneElemOneLine       :: Field f [ValueType]
                         , _elemsOnSepLine       :: Field f [ValueType]
                         } deriving (Generic)

type Config = ConfigOf Identity
type ConfigMaybe = ConfigOf Maybe

makeLenses ''ConfigOf

deriving instance Show Config
deriving instance Show ConfigMaybe
deriving instance Eq Config
deriving instance Eq ConfigMaybe

instance Default Config where
 def = Config { _spaceNBeforeColon = 0
              , _spaceNAfterColon = 1
              , _spaceNBeforeArrComma = 0
              , _spaceNAfterArrComma = 1
              , _arrPaddingSpaceN = 1
              , _spaceNInEmptyArr = 0
              , _spaceNInEmptyObj = 0
              , _objPaddingSpaceN = 1
              , _endWithNewline = True
              , _oneEntryOneLine = [Empty, Null, Bool, Number, EmptyString, NonEmptyString, EmptyArray, EmptyObject]
              , _oneElemOneLine = [Empty, Null, Bool, Number, EmptyString, NonEmptyString, EmptyObject]
              , _elemsOnSepLine = [FilledObject, FilledArray]
              }

instance FromJSON ConfigMaybe where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = drop 1
    , omitNothingFields = True
    , rejectUnknownFields = True
    }

-- | For any missing fields in the input, use the default value.
-- THIS IS SO STUPID
overrideDefaults :: ConfigMaybe -> Config
overrideDefaults input = def
  & overrideOn spaceNBeforeColon spaceNBeforeColon
  & overrideOn spaceNAfterColon spaceNAfterColon
  & overrideOn spaceNBeforeArrComma spaceNBeforeArrComma
  & overrideOn spaceNAfterArrComma spaceNAfterArrComma
  & overrideOn arrPaddingSpaceN arrPaddingSpaceN
  & overrideOn spaceNInEmptyArr spaceNInEmptyArr
  & overrideOn spaceNInEmptyObj spaceNInEmptyObj
  & overrideOn objPaddingSpaceN objPaddingSpaceN
  & overrideOn endWithNewline endWithNewline
  & overrideOn oneEntryOneLine oneEntryOneLine
  & overrideOn oneElemOneLine oneElemOneLine
  & overrideOn elemsOnSepLine elemsOnSepLine
    where overrideOn :: Lens' Config a -> Lens' ConfigMaybe (Maybe a) -> Config -> Config
          overrideOn clens mlens = set clens (fromMaybe (def ^. clens) (input ^. mlens))

-- | Parse a JSON-encoded configuration.
-- Default values will be used for missing fields.
parseConfigJSON :: ByteString -> Either String Config
parseConfigJSON = fmap overrideDefaults . eitherDecodeStrict @ConfigMaybe

