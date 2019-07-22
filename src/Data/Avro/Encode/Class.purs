module Data.Avro.Encode.Class where

import Data.ByteString (ByteString)

class ToAvro a where
  encodeAvro :: a -> ByteString