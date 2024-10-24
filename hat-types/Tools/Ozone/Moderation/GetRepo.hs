{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tools.Ozone.Moderation.GetRepo where

import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Servant.API
import {-# SOURCE #-} qualified Tools.Ozone.Moderation.Defs

newtype GetRepoResult = GetRepoResult
  { getGetRepoResult :: Tools.Ozone.Moderation.Defs.RepoViewDetail
  }

type GetRepo = "tools.ozone.moderation.getRepo" Servant.API.:> Servant.API.Header "Authorization" Data.Text.Text Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, Servant.API.Strict] "did" Data.Text.Text Servant.API.:> Servant.API.Get '[Servant.API.JSON] GetRepoResult
