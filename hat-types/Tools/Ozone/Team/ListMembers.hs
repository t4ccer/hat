{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tools.Ozone.Team.ListMembers where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API
import {-# SOURCE #-} qualified Tools.Ozone.Team.Defs

data ListMembersResult = ListMembersResult
  { cursor :: Maybe Data.Text.Text
  , members :: [Tools.Ozone.Team.Defs.Member]
  }
  deriving (Show, Read, Eq, Ord)

instance Data.Aeson.FromJSON ListMembersResult where
  parseJSON = Data.Aeson.withObject "ListMembersResult" $ \v -> do
    cursor <- v Data.Aeson..:? Data.Aeson.Key.fromString "cursor"
    members <- v Data.Aeson..: Data.Aeson.Key.fromString "members"
    pure $ ListMembersResult cursor members

instance Data.Aeson.ToJSON ListMembersResult where
  toJSON (ListMembersResult cursor members) =
    Data.Aeson.Object $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "members" Data.Aeson..= members
        ]
  toEncoding (ListMembersResult cursor members) =
    Data.Aeson.pairs $
      mconcat
        [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
        , Data.Aeson.Key.fromString "members" Data.Aeson..= members
        ]

listMembersResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListMembersResult -> kv
listMembersResult'AesonFields (ListMembersResult cursor members) =
  mconcat
    [ Data.Aeson.Key.fromString "cursor" Data.Aeson..?= cursor
    , Data.Aeson.Key.fromString "members" Data.Aeson..= members
    ]

type ListMembers = "tools.ozone.team.listMembers" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "cursor" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Optional] "limit" Integer Servant.API.:> Servant.API.Get '[Servant.API.JSON] ListMembersResult
