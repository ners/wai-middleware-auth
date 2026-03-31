{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Spec.Network.Wai.Auth.Internal (tests) where

import           Data.Binary                   (encode, decodeOrFail)
import qualified Data.ByteString.Lazy.Char8    as BSL8
import qualified Data.Text                     as T
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.Hedgehog           (testProperty)
import           Hedgehog
import           Hedgehog.Gen                  as Gen
import           Hedgehog.Range                as Range
import           Network.Wai.Auth.Internal
import qualified Network.OAuth.OAuth2          as OA2

tests :: TestTree
tests = testGroup "Network.Wai.Auth.Internal"
  [ testProperty "oAuth2TokenBinaryDuality" oAuth2TokenBinaryDuality
  ]

oAuth2TokenBinaryDuality :: Property
oAuth2TokenBinaryDuality = property $ do
  token <- forAll oauth2Token
  let checkUnconsumed ("", _, roundTripToken) = roundTripToken
      checkUnconsumed (unconsumed, _, _) =
        error $ "Unexpected unconsumed in bytes: " <> BSL8.unpack unconsumed
  tripping token encode (fmap checkUnconsumed . decodeOrFail)
  tripping token encodeToken decodeToken

oauth2Token :: Gen OA2.OAuth2Token
oauth2Token = do
  accessToken <- OA2.AccessToken <$> anyText
  refreshToken <- Gen.maybe $ OA2.RefreshToken <$> anyText
  expiresIn <- Gen.maybe $ Gen.int (Range.linear 0 1000)
  tokenType <- Gen.maybe anyText
  idToken <- Gen.maybe $ OA2.IdToken <$> anyText
  scope <- Gen.maybe anyText
  let rawResponse = mempty
  pure . fixRawResponse $ OA2.OAuth2Token accessToken refreshToken expiresIn tokenType idToken scope rawResponse

anyText :: Gen T.Text
anyText = Gen.text (Range.linear 0 100) Gen.unicodeAll
