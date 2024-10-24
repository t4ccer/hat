{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Com.Atproto.Identity.GetRecommendedDidCredentials where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API

data GetRecommendedDidCredentialsResult = GetRecommendedDidCredentialsResult
  { alsoKnownAs :: Maybe [Data.Text.Text]
  , rotationKeys :: Maybe [Data.Text.Text]
  , services :: Maybe Data.Aeson.Value
  , verificationMethods :: Maybe Data.Aeson.Value
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON GetRecommendedDidCredentialsResult where
  parseJSON = Data.Aeson.withObject "GetRecommendedDidCredentialsResult" $ \v -> do
    alsoKnownAs <- v Data.Aeson..:? Data.Aeson.Key.fromString "alsoKnownAs"
    rotationKeys <- v Data.Aeson..:? Data.Aeson.Key.fromString "rotationKeys"
    services <- v Data.Aeson..:? Data.Aeson.Key.fromString "services"
    verificationMethods <- v Data.Aeson..:? Data.Aeson.Key.fromString "verificationMethods"
    pure $ GetRecommendedDidCredentialsResult alsoKnownAs rotationKeys services verificationMethods

instance Data.Aeson.ToJSON GetRecommendedDidCredentialsResult where
  toJSON (GetRecommendedDidCredentialsResult alsoKnownAs rotationKeys services verificationMethods) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "alsoKnownAs" Data.Aeson..?= alsoKnownAs
        , Data.Aeson.Key.fromString "rotationKeys" Data.Aeson..?= rotationKeys
        , Data.Aeson.Key.fromString "services" Data.Aeson..?= services
        , Data.Aeson.Key.fromString "verificationMethods" Data.Aeson..?= verificationMethods
        ]
  toEncoding (GetRecommendedDidCredentialsResult alsoKnownAs rotationKeys services verificationMethods) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "alsoKnownAs" Data.Aeson..?= alsoKnownAs
        , Data.Aeson.Key.fromString "rotationKeys" Data.Aeson..?= rotationKeys
        , Data.Aeson.Key.fromString "services" Data.Aeson..?= services
        , Data.Aeson.Key.fromString "verificationMethods" Data.Aeson..?= verificationMethods
        ]

getRecommendedDidCredentialsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetRecommendedDidCredentialsResult -> kv
getRecommendedDidCredentialsResult'AesonFields (GetRecommendedDidCredentialsResult alsoKnownAs rotationKeys services verificationMethods) =
  mconcat
    [ Data.Aeson.Key.fromString "alsoKnownAs" Data.Aeson..?= alsoKnownAs
    , Data.Aeson.Key.fromString "rotationKeys" Data.Aeson..?= rotationKeys
    , Data.Aeson.Key.fromString "services" Data.Aeson..?= services
    , Data.Aeson.Key.fromString "verificationMethods" Data.Aeson..?= verificationMethods
    ]

type GetRecommendedDidCredentials = "com.atproto.identity.getRecommendedDidCredentials" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetRecommendedDidCredentialsResult
