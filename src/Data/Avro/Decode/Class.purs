module Data.Avro.Decode.Class where

import Data.ByteString (ByteString)

class FromAvro a where
  decodeAvro :: ByteString -> Either String a