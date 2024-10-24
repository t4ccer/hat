module Com.Atproto.Server.GetSession where

import qualified Data.Aeson

data GetSessionResult

instance Show GetSessionResult
instance Read GetSessionResult
instance Eq GetSessionResult
instance Ord GetSessionResult
instance Data.Aeson.FromJSON GetSessionResult

instance Data.Aeson.ToJSON GetSessionResult

getSessionResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetSessionResult -> kv
