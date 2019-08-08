module Test.Data.Avro.Types
  ( quickCheckAvroType
  , describeAvroType
  ) where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Avro.Types (Type)
import Data.Avro.Types as Types
import Data.Either (Either(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Test.QuickCheck (quickCheck)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

describeAvroType :: Effect Unit
describeAvroType = run [consoleReporter] do
  describe "avro-type" do
    describe "JSON serialization" do
      it "decodes primitive schemas" (traverse_ avroTypeDecodeTest avroPrims)
      it "encodes primitive schemas" (traverse_ avroTypeEncodeTest avroPrims)

  where avroTypeDecodeTest (Tuple typ input) =
          parseJson input `shouldEqual` Right typ

        avroTypeEncodeTest (Tuple typ expected) =
          toJson typ `shouldEqual` expected

parseJson :: forall a. DecodeJson a => String -> Either String a
parseJson = jsonParser >=> decodeJson

toJson :: forall a. EncodeJson a => a -> String
toJson = encodeJson >>> stringify

avroPrims :: Array (Tuple Type String)
avroPrims =
  [ Tuple Types.Null "\"null\""
  , Tuple Types.Boolean "\"boolean\""
  , Tuple Types.Int "\"int\""
  , Tuple Types.Long "\"long\""
  , Tuple Types.Float "\"float\""
  , Tuple Types.Double "\"double\""
  , Tuple Types.Bytes "\"bytes\""
  , Tuple Types.String "\"string\""
  ]

quickCheckAvroType :: Effect Unit
quickCheckAvroType = quickCheck jsonRoundTrip

jsonRoundTrip :: Type -> Boolean
jsonRoundTrip typ = decodeJson (encodeJson typ) == Right typ