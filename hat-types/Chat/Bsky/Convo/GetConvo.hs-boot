module Chat.Bsky.Convo.GetConvo where

import qualified Data.Aeson

data GetConvoResult

instance Show GetConvoResult
instance Read GetConvoResult
instance Eq GetConvoResult
instance Ord GetConvoResult
instance Data.Aeson.FromJSON GetConvoResult

instance Data.Aeson.ToJSON GetConvoResult

getConvoResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetConvoResult -> kv
