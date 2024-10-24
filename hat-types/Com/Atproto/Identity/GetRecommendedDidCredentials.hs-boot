module Com.Atproto.Identity.GetRecommendedDidCredentials where

import qualified Data.Aeson

data GetRecommendedDidCredentialsResult

instance Show GetRecommendedDidCredentialsResult
instance Read GetRecommendedDidCredentialsResult
instance Eq GetRecommendedDidCredentialsResult
instance Ord GetRecommendedDidCredentialsResult
instance Data.Aeson.FromJSON GetRecommendedDidCredentialsResult

instance Data.Aeson.ToJSON GetRecommendedDidCredentialsResult

getRecommendedDidCredentialsResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetRecommendedDidCredentialsResult -> kv
