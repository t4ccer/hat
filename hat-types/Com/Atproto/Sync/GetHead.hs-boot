module Com.Atproto.Sync.GetHead where

import qualified Data.Aeson

data GetHeadResult

instance Show GetHeadResult
instance Read GetHeadResult
instance Eq GetHeadResult
instance Ord GetHeadResult
instance Data.Aeson.FromJSON GetHeadResult

instance Data.Aeson.ToJSON GetHeadResult

getHeadResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetHeadResult -> kv
