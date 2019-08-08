module Data.Avro.Types
  ( Type(..)
  , TypeName(..)
  , Order(..)
  , Field(..)
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (class Lazy, defer)
import Control.Monad.Gen (chooseInt, elements, oneOf, resize, unfoldable) as Gen
import Control.Monad.Gen (class MonadGen, chooseBool, chooseFloat, chooseInt)
import Control.Monad.Gen.Common (genMaybe, genNonEmpty) as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut.Core (Json, caseJsonArray, caseJsonBoolean, caseJsonNull, caseJsonNumber, caseJsonObject, caseJsonString, jsonEmptyObject, stringify)
import Data.Argonaut.Core as Json
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (:=?), (~>))
import Data.Avro.Internal.Generic (foreignToJson, jsonToForeign)
import Data.Array (catMaybes)
import Data.Avro.Values (Value)
import Data.Avro.Values as Value
import Data.ByteString (ByteString)
import Data.ByteString as BS
import Data.Either (Either(..))
import Data.Foldable (foldl, elem)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.List.NonEmpty as NEL
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.NonEmpty ((:|))
import Data.String.Gen (genUnicodeString)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign (ForeignError(..), fail)
import Foreign.Class (class Encode, class Decode)
import Foreign.Object as Object
import Node.Encoding (Encoding(Base64))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)


newtype TypeName =
  TypeName String

derive instance eqTypeName :: Eq TypeName

instance showTypeName :: Show TypeName where
  show (TypeName name) = name

derive instance genericAvroTypeName :: Generic TypeName _
derive instance newtypeAvroTypeName :: Newtype TypeName _

instance decodeJsonAvroTypeName :: DecodeJson TypeName where
  decodeJson json = TypeName <$> decodeJson json

instance encodeJsonAvroTypeName :: EncodeJson TypeName where
  encodeJson (TypeName value) = encodeJson value

instance arbitraryAvroTypeName :: Arbitrary TypeName where
  arbitrary = TypeName <$> genUnicodeString

data Type =
    Null
  | Boolean
  | Int
  | Long
  | Float
  | Double
  | Bytes
  | String
  | Array { items :: Type }
  | Map { values :: Type }
  | Union { options :: NonEmptyList Type }
  | Fixed {
      name :: TypeName
    , namespace :: Maybe String
    , aliases :: List TypeName
    , size :: Int
    }
  | Enum {
      name :: TypeName
    , namespace :: Maybe String
    , aliases :: List TypeName
    , doc :: Maybe String
    , symbols :: NonEmptyList TypeName
    }
  | Record {
      name :: TypeName
    , namespace :: Maybe String
    , aliases :: List TypeName
    , doc :: Maybe String
    , order :: Maybe Order
    , fields :: List Field
    }

derive instance genericAvroType :: Generic Type _
derive instance eqAvroType :: Eq Type

instance showAvroType :: Show Type where
  show = stringify <<< encodeJson

type Decoder a = Json -> Either String a

type TypeDecoder = Decoder Type

oneOfDecoder :: forall a. Array (Decoder a) -> Decoder a
oneOfDecoder decoders =
  \json -> foldl (\prev dec -> (dec json) <|> prev) (Left "Invalid type") decoders

jsonNonEmptyList :: forall a. DecodeJson a => Decoder (NonEmptyList a)
jsonNonEmptyList json = do
  list <- decodeJson json
  case (NEL.fromList list) of
    Just ls -> Right ls
    Nothing -> Left "List can not be empty"

instance decodeJsonAvroType :: DecodeJson Type where
  decodeJson = oneOfDecoder
    [ decodeNullType
    , decodeBooleanType
    , decodeIntType
    , decodeLongType
    , decodeFloatType
    , decodeDoubleType
    , decodeBytesType
    , decodeStringType
    , decodeArrayType
    , decodeMapType
    , decodeUnionType
    , decodeFixedType
    , decodeEnumType
    , decodeRecordType
    ]

instance encodeJsonAvroType :: EncodeJson Type where
  encodeJson Null = encodeJson "null"
  encodeJson Boolean = encodeJson "boolean"
  encodeJson Int = encodeJson "int"
  encodeJson Long = encodeJson "long"
  encodeJson Float = encodeJson "float"
  encodeJson Double = encodeJson "double"
  encodeJson Bytes = encodeJson "bytes"
  encodeJson String = encodeJson "string"
  encodeJson (Array { items }) =
    "items" := items
    ~> jsonEmptyObject
  encodeJson (Map { values }) =
    "values" := values
    ~> jsonEmptyObject
  encodeJson (Union { options }) =
    encodeJson (NEL.toList options)
  encodeJson (Fixed fixed) =
    let opts = catMaybes
          [ "namespace" :=? fixed.namespace ]
    in "name" := fixed.name
       ~> "type" := "fixed"
       ~> "aliases" := fixed.aliases
       ~> "size" := fixed.size
       ~> (Json.fromObject $ Object.fromFoldable opts)
  encodeJson (Enum enum) =
    let opts = catMaybes
          [ "namespace" :=? enum.namespace
          , "doc" :=? enum.doc
          ]
    in "name" := enum.name
       ~> "type" := "enum"
       ~> "aliases" := enum.aliases
       ~> "symbols" := encodeJson (NEL.toList enum.symbols)
       ~> (Json.fromObject $ Object.fromFoldable opts)
  encodeJson (Record rec) =
    let opts = catMaybes
          [ "namespace" :=? rec.namespace
          , "doc" :=? rec.doc
          , "order" :=? rec.order
          ]
    in "name" := rec.name
       ~> "type" := "record"
       ~> "aliases" := rec.aliases
       ~> "fields" := rec.fields
       ~> (Json.fromObject $ Object.fromFoldable opts)

instance encodeForeignAvroType :: Encode Type where
  encode = encodeJson >>> jsonToForeign

instance decodeForeignAvroType :: Decode Type where
  decode f = do
    json <- foreignToJson f
    case (decodeJson json) of
      Left err -> fail $ ForeignError err
      Right x -> pure x

decodePrimitiveType :: String -> String -> Type -> TypeDecoder
decodePrimitiveType name errMsg typ json = do
  value <- decodeJson json
  if value == name then Right typ
    else Left errMsg

decodeNullType :: TypeDecoder
decodeNullType =
  decodePrimitiveType "null" "Not a null type" Null

decodeBooleanType :: TypeDecoder
decodeBooleanType =
  decodePrimitiveType "boolean" "Not a boolean type" Boolean

decodeIntType :: TypeDecoder
decodeIntType =
  decodePrimitiveType "int" "Not an int type" Int

decodeLongType :: TypeDecoder
decodeLongType =
  decodePrimitiveType "long" "Not a long type" Long

decodeFloatType :: TypeDecoder
decodeFloatType =
  decodePrimitiveType "float" "Not a float type" Float

decodeDoubleType :: TypeDecoder
decodeDoubleType =
  decodePrimitiveType "double" "Not a double type" Double

decodeBytesType :: TypeDecoder
decodeBytesType =
  decodePrimitiveType "bytes" "Not a bytes type" Bytes

decodeStringType :: TypeDecoder
decodeStringType =
  decodePrimitiveType "string" "Not an string type" String

decodeArrayType :: TypeDecoder
decodeArrayType json = do
  obj <- decodeJson json
  typ <- obj .: "items"
  pure $ Array { items: typ }

decodeMapType :: TypeDecoder
decodeMapType json = do
  obj <- decodeJson json
  typ <- obj .: "values"
  pure $ Map { values: typ }

decodeUnionType :: TypeDecoder
decodeUnionType json = do
  values <- jsonNonEmptyList json
  pure $ Union { options: values }

decodeFixedType :: TypeDecoder
decodeFixedType json = do
  obj <- decodeJson json
  name <- obj .: "name"
  namespace <- obj .:? "namespace"
  aliases <- obj .: "aliases"
  size <- obj .: "size"
  pure $ Fixed { name, namespace, aliases, size }

decodeEnumType :: TypeDecoder
decodeEnumType json = do
  obj <- decodeJson json
  name <- obj .: "name"
  namespace <- obj .:? "namespace"
  aliases <- obj .: "aliases"
  doc <- obj .:? "doc"
  maybeSymbols <- NEL.fromList <$> obj .: "symbols"
  case maybeSymbols of
    Just symbols ->
      pure $ Enum { name, namespace, aliases, doc, symbols }
    Nothing ->
      Left "Symbol list for Enum can not be empty"

decodeRecordType :: TypeDecoder
decodeRecordType json = do
  obj <- decodeJson json
  name <- obj .: "name"
  namespace <- obj .:? "namespace"
  aliases <- obj .: "aliases"
  doc <- obj .:? "doc"
  order <- obj .:? "order"
  fields <- obj .: "fields"
  pure $ Record {
    name: name
  , aliases: aliases
  , namespace: namespace
  , doc: doc
  , order: order
  , fields: fields
  }

instance arbitraryAvroType :: Arbitrary Type where
  arbitrary = Gen.oneOf (genLeaf :| [genArray, genMap, genUnion, genFixed, genEnum, genRecord])
    where genLeaf :: Gen Type
          genLeaf = Gen.elements (Null :| [Boolean, Int, Long, Float, Double, Bytes, String])

          genArray :: Gen Type
          genArray = do
            items <- defer \_ -> arbitrary
            pure $ Array { items }

          genMap :: Gen Type
          genMap = do
            values <- defer \_ -> arbitrary
            pure $ Map { values }

          genUnion :: Gen Type
          genUnion = do
            options <- NonEmptyList <$> Gen.genNonEmpty (defer \_ -> arbitrary)
            pure $ Union { options }

          genFixed :: Gen Type
          genFixed = do
            name <- arbitrary
            namespace <- Gen.genMaybe genUnicodeString
            aliases <- Gen.unfoldable arbitrary
            size <- Gen.chooseInt 0 bottom
            pure $ Fixed { name, namespace, aliases, size }

          genEnum :: Gen Type
          genEnum = do
            name <- arbitrary
            namespace <- Gen.genMaybe genUnicodeString
            aliases <- Gen.unfoldable arbitrary
            doc <- Gen.genMaybe genUnicodeString
            symbols <- NonEmptyList <$> Gen.genNonEmpty arbitrary
            pure $ Enum { name, namespace, aliases, doc, symbols }

          genRecord :: Gen Type
          genRecord = do
            name <- arbitrary
            namespace <- Gen.genMaybe genUnicodeString
            aliases <- Gen.unfoldable arbitrary
            doc <- Gen.genMaybe genUnicodeString
            order <- Gen.genMaybe arbitrary
            fields <- Gen.unfoldable arbitrary
            pure $ Record { name, namespace, aliases, doc, order, fields }

-- Order

data Order =
    Ascending
  | Descending
  | Ignore

derive instance genericAvroOrder :: Generic Order _
derive instance eqAvroOrder :: Eq Order

instance showAvroOrder :: Show Order where
  show Ascending = "ascending"
  show Descending = "descending"
  show Ignore = "ignore"

instance decodeJsonAvroOrder :: DecodeJson Order where
  decodeJson json = do
    value <- decodeJson json
    case value of
      "ascending" -> Right Ascending
      "descending" -> Right Descending
      "ignore" -> Right Ignore
      _ -> Left $ "Not a valid order value: " <> value

instance encodeJsonAvroOrder :: EncodeJson Order where
  encodeJson Ascending = encodeJson "ascending"
  encodeJson Descending = encodeJson "descending"
  encodeJson Ignore = encodeJson "ignore"

instance arbitraryAvroOrder :: Arbitrary Order where
  arbitrary = Gen.elements (Ascending :| [Descending, Ignore])

-- Field

data Field = Field {
    name :: String
  , aliases :: List String
  , doc :: Maybe String
  , order :: Maybe Order
  , typ :: Type
  , default :: (Maybe (Value Type))
  }

derive instance genericAvroField :: Generic Field _
derive instance eqAvroField :: Eq Field

instance decodeJsonAvroField :: DecodeJson Field where
  decodeJson json = do
    obj <- decodeJson json
    name <- obj .: "name"
    aliases <- obj .: "aliases"
    doc <- obj .:? "doc"
    order <- obj .:? "order"
    typ <- obj .: "type"
    def <- do
      defJson <- obj .:? "default"
      traverse (parseFieldDefault typ) defJson
    pure $ Field { name: name, aliases: aliases, doc: doc, order: order, typ: typ, default: def }

instance encodeJsonAvroField :: EncodeJson Field where
  encodeJson (Field field) =
    "name" := field.name
    ~> "aliases" := field.aliases
    ~> "doc" := field.doc
    ~> "order" := field.order
    ~> "type" := field.typ
    ~> "default" := (encodeJsonValue <$> field.default)
    ~> jsonEmptyObject

instance arbitraryAvroField :: Arbitrary Field where
  arbitrary = do
    name <- genUnicodeString
    aliases <- Gen.unfoldable genUnicodeString
    doc <- Gen.genMaybe genUnicodeString
    order <- Gen.genMaybe arbitrary
    typ <- (defer \_ -> arbitrary)
    default <- Gen.genMaybe $ genValue typ
    pure $ Field
      { name, aliases, doc, order, typ, default }

-- | Parses the default value of a field definition in an schema
parseFieldDefault :: Type -> Json -> Either String (Value Type)
parseFieldDefault Null =
  caseJsonNull (Left "Not a null value") (\_ -> Right Value.Null)

parseFieldDefault Boolean =
  caseJsonBoolean (Left "Not a boolean value") (Right <<< Value.Boolean)

parseFieldDefault Int =
  \x -> do
    v <- decodeJson x
    Right $ Value.Int v

parseFieldDefault Long =
  \x -> do
    v <- decodeJson x
    Right $ Value.Long v

parseFieldDefault Float =
  caseJsonNumber (Left "Not an float value") (Right <<< Value.Float)

parseFieldDefault Double =
  caseJsonNumber (Left "Not an double value") (Right <<< Value.Double)

parseFieldDefault Bytes =
  \json -> do
    str <- decodeJson json
    case (BS.fromString str Base64) of
      Just x -> Right $ Value.Bytes x
      Nothing -> Left "Invalid btes encoding"

parseFieldDefault String =
  caseJsonString (Left "Not an string value") (Right <<< Value.String)

parseFieldDefault (Array { items }) =
  caseJsonArray (Left "Not an array value") parseArray
  where parseArray arr =
          Value.Array <$> traverse (parseFieldDefault items) arr

parseFieldDefault (Map { values }) =
  caseJsonObject (Left "Not a map value") parseMap
  where parseMap obj = do
          parsedObj <- traverse (parseFieldDefault values) obj
          tupled <- pure $ ((Object.toUnfoldable parsedObj) :: Array (Tuple String (Value Type)))
          pure $ Value.Map (Map.fromFoldable tupled)

parseFieldDefault t@(Record { fields }) =
  caseJsonObject (Left "Not a record value") parseRecord
  where lookupAndParseField valueMap (Field { name, typ }) =
          case (Map.lookup name valueMap) of
            Just v -> map (\x -> Tuple name x) $ parseFieldDefault typ v
            Nothing -> Left $ "Field not found: " <> name

        parseRecord obj =
          let tupled = (Object.toUnfoldable obj) :: Array (Tuple String Json)
              valueMap = Map.fromFoldable tupled
          in (Value.Record t) <$> Map.fromFoldable <$> traverse (lookupAndParseField valueMap) fields

parseFieldDefault typ@(Union { options }) =
  \json -> do
    v <- foldl (\prev t -> prev <|> parseFieldDefault t json) (Left "Not a valid union value") options
    pure $ Value.Union options typ v

parseFieldDefault typ@(Enum { name, symbols }) =
  caseJsonString (Left "No a valid enum value") checkEnum
  where checkEnum v =
          if (elem (TypeName v) symbols) then Right $ Value.Enum typ v
            else Left $ "Invalid value '" <> v <> "' for enum: " <> (show name)

parseFieldDefault typ@(Fixed { size }) =
  \json -> do
    parsed <- parseFieldDefault Bytes json
    case parsed of
      Value.Bytes bytes ->
          if (BS.length bytes > size) then
            Left $ "Bytes received exceed the size: " <> (show size)
            else Right $ Value.Fixed typ bytes
      _ -> Left "IMPOSSIBLE BUG!"

-- |Encodes an Avro Value into Json, needed by the schema field definition
encodeJsonValue :: Value Type -> Json
encodeJsonValue Value.Null = encodeJson "null"
encodeJsonValue (Value.Boolean v) = encodeJson v
encodeJsonValue (Value.Int v) = encodeJson v
encodeJsonValue (Value.Long v) = encodeJson v
encodeJsonValue (Value.Float v) = encodeJson v
encodeJsonValue (Value.Double v) = encodeJson v
encodeJsonValue (Value.Bytes v) = encodeJson $ BS.toString v Base64
encodeJsonValue (Value.String v) = encodeJson v
encodeJsonValue (Value.Array v) = encodeJson $ map encodeJsonValue v
encodeJsonValue (Value.Map v) = encodeJson $ map encodeJsonValue v
encodeJsonValue (Value.Fixed _ v) = encodeJson $ BS.toString v Base64
encodeJsonValue (Value.Enum _ v) = encodeJson v
encodeJsonValue (Value.Union _ _ v) = encodeJsonValue v
encodeJsonValue (Value.Record _ v) = encodeJson $ map encodeJsonValue v

-- | Generate an arbitrary typed value
genIntegerValue :: forall m t. MonadGen m => (Int -> Value t) -> m (Value t)
genIntegerValue f = f <$> chooseInt top bottom

genNumberValue :: forall m t. MonadGen m => (Number -> Value t) -> m (Value t)
genNumberValue f = f <$> chooseFloat top bottom

genByteString :: forall m. MonadGen m => MonadRec m => m ByteString
genByteString = BS.toUTF8 <$> genUnicodeString

-- | Generate an arbitrary typed value
genValue :: forall m. MonadGen m => MonadRec m => Lazy (m (Value Type)) => Type -> m (Value Type)
genValue Null = pure Value.Null
genValue Boolean = Value.Boolean <$> chooseBool
genValue Int = genIntegerValue Value.Int
genValue Long = genIntegerValue Value.Long
genValue Float = genNumberValue Value.Float
genValue Double = genNumberValue Value.Double
genValue Bytes = Value.Bytes <$> genByteString
genValue String = Value.String <$> genUnicodeString
genValue (Array { items }) = Value.Array <$> (Gen.unfoldable $ (defer \_ -> genValue items))
genValue (Map { values }) = do
  vs <- (Gen.unfoldable (Tuple <$> genUnicodeString <*> (defer \_ -> genValue values))) :: m (Array (Tuple String (Value Type)))
  let valueMap = Map.fromFoldable vs
  pure $ Value.Map valueMap
genValue typ@(Union { options }) = do
  opts <- traverse (defer \_ -> genValue) options
  value <- Gen.elements opts
  pure $ Value.Union options typ value
genValue typ@(Fixed { size }) =
  Value.Fixed typ <$> Gen.resize (\_ -> size) genByteString
genValue typ@(Enum { symbols }) = ((Value.Enum typ) <<< show) <$> Gen.elements symbols
genValue t@(Record { fields }) = do
  values <- traverse genFieldValue fields
  let valueMap = Map.fromFoldable values
  pure $ Value.Record t valueMap
  where genFieldValue :: Field -> m (Tuple String (Value Type))
        genFieldValue (Field { name, typ }) =
          Tuple <$> (pure name) <*> (defer \_ -> genValue typ)
          