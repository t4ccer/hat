module Chat.Bsky.Convo.ListConvos where

import qualified Data.Aeson

data ListConvosResult

instance Show ListConvosResult
instance Read ListConvosResult
instance Eq ListConvosResult
instance Ord ListConvosResult
instance Data.Aeson.FromJSON ListConvosResult

instance Data.Aeson.ToJSON ListConvosResult

listConvosResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ListConvosResult -> kv
