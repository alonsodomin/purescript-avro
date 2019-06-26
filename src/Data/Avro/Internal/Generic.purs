module Data.Avro.Internal.Generic
  ( jsonToForeign
  , foreignToJson
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core (Json, caseJson, jsonNull)
import Data.Argonaut.Core as Json
import Data.Traversable (traverse)
import Foreign (F, Foreign, ForeignError(..), fail, isArray, isNull, isUndefined, readArray, readBoolean, readNumber, readString, tagOf, unsafeFromForeign, unsafeToForeign)
import Foreign.Object (Object)
import Foreign.Object as Object

jsonToForeign :: Json -> Foreign
jsonToForeign = caseJson
  (const $ unsafeToForeign {})
  (\b -> unsafeToForeign b)
  (\x -> unsafeToForeign x)
  (\s -> unsafeToForeign s)
  (\xs -> unsafeToForeign $ map jsonToForeign xs)
  (\obj -> unsafeToForeign $ Object.mapWithKey (\_ -> jsonToForeign) obj)

foreignToJson :: Foreign -> F Json
foreignToJson thing
  | (isNull thing || isUndefined thing) = pure jsonNull
  | isArray thing = do
      elems <- readArray thing
      arr <- traverse foreignToJson elems
      pure $ Json.fromArray arr
  | isForeignObject thing = do
      obj <- readForeignObject thing
      decodedObj <- traverse foreignToJson obj
      pure $ Json.fromObject decodedObj
  | otherwise =
          (Json.fromBoolean <$> (readBoolean thing))
      <|> (Json.fromNumber <$> (readNumber thing))
      <|> (Json.fromString <$> (readString thing))


isForeignObject :: Foreign -> Boolean
isForeignObject v = tagOf v == "Object"

readForeignObject :: Foreign -> F (Object Foreign)
readForeignObject value
  | isForeignObject value = pure $ unsafeFromForeign value
  | otherwise = fail $ TypeMismatch "Object" (tagOf value)