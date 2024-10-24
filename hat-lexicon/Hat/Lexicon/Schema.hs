-- | Datatype and JSON parsers for Lexicon schema files
module Hat.Lexicon.Schema (
  Lexicon (..),
  Definition (..),
  DefObject (..),
  DefInteger (..),
  DefString (..),
  DefArray (..),
  DefBoolean (..),
  DefUnion (..),
  DefBlob (..),
  DefQuery (..),
  DefProcedure (..),
  DefToken (..),
  DefBytes (..),
  DefCidLink (..),
  DefSubscription (..),
  DefRecord (..),
  DefRecordKey (..),
  DefMessage (..),
  DefParameters (..),
  DefError (..),
  InputOutput (..),
) where

import Control.Monad (guard)
import Data.Aeson (FromJSON (parseJSON), KeyValueOmit, ToJSON, withObject, withText, (.:), (.:?), (.=), (.?=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Encoding
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

{- | Lexicon is a JSON file associated with a single NSID.
A file contains one or more definitions, each with a distinct short name
-}
data Lexicon = Lexicon
  { lexiconId :: ByteString
  -- ^ NSID of the Lexicon
  , lexiconDefinitions :: Map ByteString Definition
  -- ^ Set of definitions, each with a distinct name (key)
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON Lexicon where
  parseJSON = withObject "Lexicon" $ \v -> do
    version <- v .: "lexicon"
    guard (version == (1 :: Int))
    lexiconId <- Text.encodeUtf8 <$> v .: "id"
    lexiconDefinitions <- Map.mapKeys Text.encodeUtf8 <$> v .: "defs"
    pure $
      Lexicon
        { lexiconId
        , lexiconDefinitions
        }

instance ToJSON Lexicon where
  toJSON (Lexicon lexiconId lexiconDefinitions) =
    Aeson.object
      [ "lexicon" .= (1 :: Int)
      , "id" .= Text.decodeUtf8 lexiconId
      , "defs" .= Map.mapKeys Text.decodeUtf8 lexiconDefinitions
      ]

  toEncoding (Lexicon lexiconId lexiconDefinitions) =
    Encoding.pairs
      ( "lexicon" .= (1 :: Int)
          <> "id" .= Text.decodeUtf8 lexiconId
          <> "defs" .= Map.mapKeys Text.decodeUtf8 lexiconDefinitions
      )

-- | Schema definition
data Definition
  = -- | Object data model type
    DefinitionObject DefObject
  | -- | Meta-definition that refers to other definition's name
    DefinitionRef ByteString
  | -- | Integer data model type
    DefinitionInteger DefInteger
  | -- | String data model type
    DefinitionString DefString
  | -- | Array data model type
    DefinitionArray DefArray
  | -- | Boolean data model type
    DefinitionBoolean DefBoolean
  | -- | Tagged union of other definitins
    DefinitionUnion DefUnion
  | -- | Blob data model type
    DefinitionBlob DefBlob
  | -- | XRPC Query (HTTP GET)
    DefinitionQuery DefQuery
  | -- | XRPC Procedure (HTTP POST)
    DefinitionProcedure DefProcedure
  | -- | Empty value, refered to only by name
    DefinitionToken DefToken
  | -- | Raw bytes. In JSON objects these are base64 encoded
    DefinitionBytes DefBytes
  | -- | CID link
    DefinitionCidLink DefCidLink
  | -- | Event Stream (WebSocket)
    DefinitionSubscription DefSubscription
  | -- | Object that can be stored in a repository record
    DefinitionRecord DefRecord
  | -- | Any data object
    DefinitionUnknown
  deriving stock (Show, Eq, Ord)

instance FromJSON Definition where
  parseJSON = withObject "Definition" $ \v -> do
    ty <- v .: "type"
    case ty of
      "object" -> DefinitionObject <$> parseJSON (Aeson.Object v)
      "ref" -> DefinitionRef . Text.encodeUtf8 <$> v .: "ref"
      "string" -> DefinitionString <$> parseJSON (Aeson.Object v)
      "integer" -> DefinitionInteger <$> parseJSON (Aeson.Object v)
      "array" -> DefinitionArray <$> parseJSON (Aeson.Object v)
      "boolean" -> DefinitionBoolean <$> parseJSON (Aeson.Object v)
      "union" -> DefinitionUnion <$> parseJSON (Aeson.Object v)
      "blob" -> DefinitionBlob <$> parseJSON (Aeson.Object v)
      "query" -> DefinitionQuery <$> parseJSON (Aeson.Object v)
      "procedure" -> DefinitionProcedure <$> parseJSON (Aeson.Object v)
      "unknown" -> pure DefinitionUnknown
      "token" -> DefinitionToken <$> parseJSON (Aeson.Object v)
      "bytes" -> DefinitionBytes <$> parseJSON (Aeson.Object v)
      "cid-link" -> DefinitionCidLink <$> parseJSON (Aeson.Object v)
      "subscription" -> DefinitionSubscription <$> parseJSON (Aeson.Object v)
      "record" -> DefinitionRecord <$> parseJSON (Aeson.Object v)
      _ -> fail ty

instance ToJSON Definition where
  toJSON = Aeson.Object . definitionFields
  toEncoding = Encoding.pairs . definitionFields

definitionFields :: (KeyValueOmit e kv, Monoid kv) => Definition -> kv
definitionFields = \case
  DefinitionObject obj -> ("type" .= ("object" :: Text)) <> defObjectFields obj
  DefinitionRef ref -> ("type" .= ("ref" :: Text)) <> "ref" .= Text.decodeUtf8 ref
  DefinitionString str -> ("type" .= ("string" :: Text)) <> defStringFields str
  DefinitionInteger int -> ("type" .= ("integer" :: Text)) <> defIntegerFields int
  DefinitionArray arr -> ("type" .= ("array" :: Text)) <> defArrayFields arr
  DefinitionBoolean bool -> ("type" .= ("boolean" :: Text)) <> defBooleanFields bool
  DefinitionUnion union -> ("type" .= ("union" :: Text)) <> defUnionFields union
  DefinitionBlob blob -> ("type" .= ("blob" :: Text)) <> defBlobFields blob
  DefinitionQuery query -> ("type" .= ("query" :: Text)) <> defQueryFields query
  DefinitionProcedure procedure -> ("type" .= ("procedure" :: Text)) <> defProcedureFields procedure
  DefinitionUnknown -> "type" .= ("unknown" :: Text)
  DefinitionToken token -> ("type" .= ("token" :: Text)) <> defTokenFields token
  DefinitionBytes bytes -> ("type" .= ("bytes" :: Text)) <> defBytesFields bytes
  DefinitionCidLink cidLink -> ("type" .= ("cid-link" :: Text)) <> defCidLinkFields cidLink
  DefinitionSubscription subscription -> ("type" .= ("subscription" :: Text)) <> defSubscriptionFields subscription
  DefinitionRecord record -> ("type" .= ("record" :: Text)) <> defRecordFields record

-- | Object data model type
data DefObject = DefObject
  { required :: [ByteString]
  -- ^ Indicates which properties are required
  , nullable :: [ByteString]
  -- ^ indicates which properties can have `null` as a value
  , properties :: Map ByteString Definition
  -- ^ Defines the properties (fields) by name, each with their own schema
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON DefObject where
  parseJSON = withObject "DefObject" $ \v -> do
    required <- maybe [] (map Text.encodeUtf8) <$> v .:? "required"
    nullable <- maybe [] (map Text.encodeUtf8) <$> v .:? "nullable"
    properties <- Map.mapKeys Text.encodeUtf8 <$> v .: "properties"
    pure $
      DefObject
        { required
        , nullable
        , properties
        }

defObjectFields :: (KeyValueOmit e kv, Monoid kv) => DefObject -> kv
defObjectFields (DefObject required nullable properties) =
  mconcat
    [ "required" .= map Text.decodeUtf8 required
    , "nullable" .= map Text.decodeUtf8 nullable
    , "properties" .= Map.mapKeys Text.decodeUtf8 properties
    ]

-- | Integer data model type
data DefInteger = DefInteger
  { minimum :: Maybe Integer
  -- ^ Minimum acceptable value
  , maximum :: Maybe Integer
  -- ^ Maximum acceptable value
  , enum :: Maybe [Integer]
  -- ^ Closed set of allowed values
  , default' :: Maybe Integer
  -- ^ Default value for this field
  , const :: Maybe Integer
  -- ^ Fixed (constant) value for this field
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON DefInteger where
  parseJSON = withObject "DefInteger" $ \v -> do
    minimum' <- v .:? "minimum"
    maximum' <- v .:? "maximum"
    enum <- v .:? "enum"
    default' <- v .:? "default"
    const' <- v .:? "const"
    pure $
      DefInteger
        { minimum = minimum'
        , maximum = maximum'
        , enum
        , default'
        , const = const'
        }

defIntegerFields :: (KeyValueOmit e kv, Monoid kv) => DefInteger -> kv
defIntegerFields (DefInteger minimum' maximum' enum default' const') =
  mconcat
    [ "minimum" .?= minimum'
    , "maximum" .?= maximum'
    , "enum" .?= enum
    , "default" .?= default'
    , "const" .?= const'
    ]

-- | String data model type
data DefString = DefString
  { format :: Maybe Text
  -- ^ String format restriction
  , maxLength :: Maybe Integer
  -- ^ Maximum length of value, in UTF-8 bytes
  , minLength :: Maybe Integer
  -- ^ Minimum length of value, in UTF-8 bytes
  , maxGraphemes :: Maybe Integer
  -- ^ Maximum length of value, counted as Unicode Grapheme Clusters
  , minGraphemes :: Maybe Integer
  -- ^ Minimum length of value, counted as Unicode Grapheme Clusters
  , enum :: Maybe [Text]
  -- ^ Closed set of allowed values
  , default' :: Maybe Text
  -- ^ Default value for this field
  , const :: Maybe Text
  -- ^ Fixed (constant) value for this field
  , knownValues :: Maybe [Text]
  -- ^ Set of suggested or common values for this field. Values are not limited to this set (aka, not a closed enum).
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON DefString where
  parseJSON = withObject "DefString" $ \v -> do
    format <- v .:? "format"
    maxLength <- v .:? "maxLength"
    minLength <- v .:? "minLength"
    maxGraphemes <- v .:? "maxGraphemes"
    minGraphemes <- v .:? "minGraphemes"
    enum <- v .:? "enum"
    default' <- v .:? "default"
    const' <- v .:? "const"
    knownValues <- v .:? "knownValues"
    pure $
      DefString
        { format
        , maxLength
        , minLength
        , maxGraphemes
        , minGraphemes
        , enum
        , default'
        , const = const'
        , knownValues
        }

defStringFields :: (KeyValueOmit e kv, Monoid kv) => DefString -> kv
defStringFields (DefString format maxLength minLength maxGraphemes minGraphemes enum default' const' knownValues) =
  mconcat
    [ "format" .?= format
    , "maxLength" .?= maxLength
    , "minLength" .?= minLength
    , "maxGraphemes" .?= maxGraphemes
    , "minGraphemes" .?= minGraphemes
    , "enum" .?= enum
    , "default" .?= default'
    , "const" .?= const'
    , "knownValues" .?= knownValues
    ]

-- | Array data model type
data DefArray = DefArray
  { items :: Definition
  -- ^ Describes the schema elements of this array
  , minLength :: Maybe Integer
  -- ^ Minimum count of elements in array
  , maxLength :: Maybe Integer
  -- ^ Maximum count of elements in array
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON DefArray where
  parseJSON = withObject "DefArray" $ \v -> do
    items <- v .: "items"
    minLength <- v .:? "minLength"
    maxLength <- v .:? "maxLength"
    pure $
      DefArray
        { items
        , minLength
        , maxLength
        }

defArrayFields :: (KeyValueOmit e kv, Monoid kv) => DefArray -> kv
defArrayFields (DefArray items minLength maxLength) =
  mconcat
    [ "items" .= items
    , "minLength" .?= minLength
    , "maxLength" .?= maxLength
    ]

-- | Boolean data model type
data DefBoolean = DefBoolean
  { default' :: Maybe Bool
  -- ^ Default value for this field
  , const :: Maybe Bool
  -- ^ Fixed (constant) value for this field
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON DefBoolean where
  parseJSON = withObject "DefBoolean" $ \v -> do
    default' <- v .:? "deafult"
    const' <- v .:? "const"
    pure $
      DefBoolean
        { default'
        , const = const'
        }

defBooleanFields :: (KeyValueOmit e kv, Monoid kv) => DefBoolean -> kv
defBooleanFields (DefBoolean default' const') =
  mconcat
    [ "default" .?= default'
    , "const" .?= const'
    ]

-- | Tagged union of other definitins
data DefUnion = DefUnion
  { refs :: [ByteString]
  -- ^ References to schema definitions
  , closed :: Maybe Bool
  -- ^ Indicates if a union is "open" or "closed".
  --
  -- By default unions are "open", meaning that future revisions of the schema could add more types
  -- to the list of refs (though can not remove types)
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON DefUnion where
  parseJSON = withObject "DefUnion" $ \v -> do
    refs <- map Text.encodeUtf8 <$> v .: "refs"
    closed <- v .:? "closed"
    pure $
      DefUnion
        { refs
        , closed
        }

defUnionFields :: (KeyValueOmit e kv, Monoid kv) => DefUnion -> kv
defUnionFields (DefUnion refs closed) =
  mconcat
    [ "refs" .= map Text.decodeUtf8 refs
    , "closed" .?= closed
    ]

-- | Blob data model type
data DefBlob = DefBlob
  { accept :: Maybe [Text]
  -- ^ List of acceptable MIME types. Each may end in * as a glob pattern (eg, image/*).
  -- Use */* to indicate that any MIME type is accepted
  , maxSize :: Maybe Integer
  -- ^ Maximum size in bytes
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON DefBlob where
  parseJSON = withObject "DefBlob" $ \v -> do
    accept <- v .:? "accept"
    maxSize <- v .:? "maxSize"
    pure $
      DefBlob
        { accept
        , maxSize
        }

defBlobFields :: (KeyValueOmit e kv, Monoid kv) => DefBlob -> kv
defBlobFields (DefBlob accept maxSize) =
  mconcat
    [ "accept" .?= accept
    , "maxSize" .?= maxSize
    ]

-- | Schema for procedure and queries outputs and inputs
data InputOutput = InputOutput
  { encoding :: Text
  -- ^ MIME type for body contents. Use application/json for JSON responses.
  , schema :: Maybe Definition
  -- ^ Schema definition, either an object, a ref, or a union of refs.
  -- Used to describe JSON encoded responses, though schema is optional even for JSON responses.
  , description :: Maybe Text
  -- ^ Short description
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON InputOutput where
  parseJSON = withObject "InputOutput" $ \v -> do
    encoding <- v .: "encoding"
    schema <- v .:? "schema"
    description <- v .:? "description"
    pure $
      InputOutput
        { encoding
        , schema
        , description
        }

instance ToJSON InputOutput where
  toJSON = Aeson.Object . defInputOutputFields
  toEncoding = Encoding.pairs . defInputOutputFields

defInputOutputFields :: (KeyValueOmit e kv, Monoid kv) => InputOutput -> kv
defInputOutputFields (InputOutput encoding schema description) =
  mconcat
    [ "encoding" .= encoding
    , "schema" .?= schema
    , "description" .?= description
    ]

-- | Error returned from query or procedure
data DefError = DefError
  { name :: Text
  -- ^ Short name for the error type, with no whitespace
  , description :: Maybe Text
  -- ^ Short description, one or two sentences
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON DefError where
  parseJSON = withObject "DefError" $ \v -> do
    name <- v .: "name"
    description <- v .:? "description"
    pure $
      DefError
        { name
        , description
        }

instance ToJSON DefError where
  toJSON = Aeson.Object . defErrorFields
  toEncoding = Encoding.pairs . defErrorFields

defErrorFields :: (KeyValueOmit e kv, Monoid kv) => DefError -> kv
defErrorFields (DefError name description) =
  mconcat
    [ "name" .= name
    , "description" .?= description
    ]

-- | Parameters of query or procedure
newtype DefParameters = DefParameters DefObject
  deriving stock (Show, Eq, Ord)

instance FromJSON DefParameters where
  parseJSON = withObject "DefParameters" $ \v -> do
    ty :: Text <- v .: "type"
    guard (ty == "params")
    obj <- parseJSON (Aeson.Object v)
    pure $ DefParameters obj

instance ToJSON DefParameters where
  toJSON = Aeson.Object . defParametersFields
  toEncoding = Encoding.pairs . defParametersFields

defParametersFields :: (KeyValueOmit e kv, Monoid kv) => DefParameters -> kv
defParametersFields (DefParameters obj) =
  "type" .= ("params" :: Text) <> defObjectFields obj

-- | XRPC Query (HTTP GET)
data DefQuery = DefQuery
  { parameters :: Maybe DefParameters
  -- ^ Schema definition with type params, describing the HTTP query parameters for this endpoint
  , output :: Maybe InputOutput
  -- ^ Describes the HTTP response body
  , errors :: Maybe [DefError]
  -- ^ Set of string error codes which might be returned
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON DefQuery where
  parseJSON = withObject "DefQuery" $ \v -> do
    parameters <- v .:? "parameters"
    output <- v .:? "output"
    errors <- v .:? "errors"
    pure $
      DefQuery
        { parameters
        , output
        , errors
        }

defQueryFields :: (KeyValueOmit e kv, Monoid kv) => DefQuery -> kv
defQueryFields (DefQuery parameters output errors) =
  mconcat
    [ "parameters" .?= parameters
    , "output" .?= output
    , "errors" .?= errors
    ]

-- | XRPC Procedure (HTTP POST)
data DefProcedure = DefProcedure
  { parameters :: Maybe DefParameters
  -- ^ Schema definition with type params, describing the HTTP query parameters for this endpoint
  , output :: Maybe InputOutput
  -- ^ Describes the HTTP response body
  , input :: Maybe InputOutput
  -- ^ Describes HTTP request body schema
  , errors :: Maybe [DefError]
  -- ^ Set of string error codes which might be returned
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON DefProcedure where
  parseJSON = withObject "DefQuery" $ \v -> do
    parameters <- v .:? "parameters"
    output <- v .:? "output"
    input <- v .:? "input"
    errors <- v .:? "errors"
    pure $
      DefProcedure
        { parameters
        , output
        , input
        , errors
        }

defProcedureFields :: (KeyValueOmit e kv, Monoid kv) => DefProcedure -> kv
defProcedureFields (DefProcedure parameters output input errors) =
  mconcat
    [ "parameters" .?= parameters
    , "output" .?= output
    , "input" .?= input
    , "errors" .?= errors
    ]

{- | Tokens are empty data values which exist only to be referenced by name.
They are used to define a set of values with specific meanings.
-}
newtype DefToken = DefToken
  { description :: Maybe Text
  -- ^ Field should clarify the meaning of the token
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON DefToken where
  parseJSON = withObject "DefToken" $ \v -> do
    description <- v .:? "description"
    pure $
      DefToken
        { description
        }

defTokenFields :: (KeyValueOmit e kv, Monoid kv) => DefToken -> kv
defTokenFields (DefToken description) = "description" .?= description

-- | Raw bytes
data DefBytes = DefBytes
  { minLength :: Maybe Integer
  -- ^ Minimum size of value, as raw bytes with no encoding
  , maxLength :: Maybe Integer
  -- ^ Maximum size of value, as raw bytes with no encoding
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON DefBytes where
  parseJSON = withObject "DefBytes" $ \v -> do
    minLength <- v .:? "minLength"
    maxLength <- v .:? "maxLength"
    pure $
      DefBytes
        { minLength
        , maxLength
        }

defBytesFields :: (KeyValueOmit e kv, Monoid kv) => DefBytes -> kv
defBytesFields (DefBytes minLength maxLength) =
  mconcat
    [ "minLength" .?= minLength
    , "maxLength" .?= maxLength
    ]

-- | CID link
newtype DefCidLink = DefCidLink
  { description :: Maybe Text
  -- ^ Short description
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON DefCidLink where
  parseJSON = withObject "DefCidLink" $ \v -> do
    description <- v .:? "description"
    pure $
      DefCidLink
        { description
        }

defCidLinkFields :: (KeyValueOmit e kv, Monoid kv) => DefCidLink -> kv
defCidLinkFields (DefCidLink description) = "description" .?= description

-- | Message returned from subscriptions
data DefMessage = DefMessage
  { schema :: Definition
  -- ^ Schema definition, which must be a union of refs
  , description :: Maybe Text
  -- ^ Short description
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON DefMessage where
  parseJSON = withObject "DefMessage" $ \v -> do
    schema <- v .: "schema"
    description <- v .:? "description"
    pure $
      DefMessage
        { schema
        , description
        }

instance ToJSON DefMessage where
  toJSON = Aeson.Object . defMesageFields
  toEncoding = Encoding.pairs . defMesageFields

defMesageFields :: (KeyValueOmit e kv, Monoid kv) => DefMessage -> kv
defMesageFields (DefMessage schema description) =
  mconcat
    [ "schema" .= schema
    , "description" .?= description
    ]

-- | Event Stream (WebSocket)
data DefSubscription = DefSubscription
  { parameters :: Maybe DefParameters
  -- ^ Schema definition with type params
  , message :: Maybe DefMessage
  -- ^ Specifies what messages can be
  , errors :: Maybe [DefError]
  -- ^ Set of string error codes which might be returned
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON DefSubscription where
  parseJSON = withObject "DefSubscription" $ \v -> do
    parameters <- v .:? "parameters"
    message <- v .:? "message"
    errors <- v .:? "errors"
    pure $
      DefSubscription
        { parameters
        , message
        , errors
        }

defSubscriptionFields :: (KeyValueOmit e kv, Monoid kv) => DefSubscription -> kv
defSubscriptionFields (DefSubscription parameters message errors) =
  mconcat
    [ "parameters" .?= parameters
    , "message" .?= message
    , "errors" .?= errors
    ]

{- | Key of records, used to name and reference an individual record within the same
collection of an atproto repository
-}
data DefRecordKey
  = -- | 64-bit timestamp identifier
    DefRecordKeyTid
  | -- | constant string, used when there should be only a single record in the collection
    DefRecordKeyLiteral Text
  | -- | Any string
    DefRecordKeyAny
  deriving stock (Show, Eq, Ord)

instance FromJSON DefRecordKey where
  parseJSON = withText "DefRecordKey" $ \case
    "tid" -> pure DefRecordKeyTid
    "any" -> pure DefRecordKeyAny
    literal ->
      if "literal:" `Text.isPrefixOf` literal
        then pure $ DefRecordKeyLiteral (Text.drop 8 literal)
        else fail "Invalid DefRecordKey"

instance ToJSON DefRecordKey where
  toJSON = \case
    DefRecordKeyTid -> "tid"
    DefRecordKeyAny -> "any"
    DefRecordKeyLiteral literal -> Aeson.String ("literal:" <> literal)
  toEncoding = \case
    DefRecordKeyTid -> Encoding.text "tid"
    DefRecordKeyAny -> Encoding.text "any"
    DefRecordKeyLiteral literal -> Encoding.text ("literal:" <> literal)

-- | Object that can be stored in a repository record
data DefRecord = DefRecord
  { key :: DefRecordKey
  -- ^ Record key
  , record :: Definition
  -- ^ Record schema
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON DefRecord where
  parseJSON = withObject "DefRecord" $ \v -> do
    key <- v .: "key"
    record <- v .: "record"
    pure $
      DefRecord
        { key
        , record
        }

defRecordFields :: (KeyValueOmit e kv, Monoid kv) => DefRecord -> kv
defRecordFields (DefRecord key record) =
  mconcat
    [ "key" .= key
    , "record" .= record
    ]
