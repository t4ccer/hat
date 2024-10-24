module Chat.Bsky.Convo.SendMessageBatch where

import qualified Data.Aeson

data BatchItem

instance Show BatchItem
instance Read BatchItem
instance Eq BatchItem
instance Ord BatchItem
instance Data.Aeson.FromJSON BatchItem

instance Data.Aeson.ToJSON BatchItem

batchItem'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => BatchItem -> kv
