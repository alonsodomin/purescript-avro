module Data.Avro.Decode.Class where

import Data.ByteString (ByteString)
import Data.Either (Either)

class FromAvro a where
  decodeAvro :: ByteString -> Either String a