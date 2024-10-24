module App.Bsky.Feed.GetQuotes where

import qualified Data.Aeson

data GetQuotesResult

instance Show GetQuotesResult
instance Read GetQuotesResult
instance Eq GetQuotesResult
instance Ord GetQuotesResult
instance Data.Aeson.FromJSON GetQuotesResult

instance Data.Aeson.ToJSON GetQuotesResult

getQuotesResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetQuotesResult -> kv
