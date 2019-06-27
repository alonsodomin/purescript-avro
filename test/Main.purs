module Test.Main where

import Prelude

import Effect (Effect)
import Test.Data.Avro.Types (describeAvroType)

main :: Effect Unit
main = describeAvroType