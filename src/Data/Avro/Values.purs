module Data.Avro.Values
  ( Value(..)
  ) where

import Prelude
import Data.ByteString (ByteString)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (NonEmptyList)
import Data.Map (Map)

data Value t =
    Null
  | Boolean Boolean
  | Int Int
  | Long Int
  | Float Number
  | Double Number
  | Bytes ByteString
  | String String
  | Array (Array (Value t))
  | Map (Map String (Value t))
  | Record t (Map String (Value t))
  | Union (NonEmptyList t) t (Value t)
  | Fixed t ByteString
  | Enum t String

derive instance eqAvroValue :: Eq t => Eq (Value t)
derive instance genericAvroValue :: Generic (Value t) _