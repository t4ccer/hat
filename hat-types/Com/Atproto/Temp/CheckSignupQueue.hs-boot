module Com.Atproto.Temp.CheckSignupQueue where

import qualified Data.Aeson

data CheckSignupQueueResult

instance Show CheckSignupQueueResult
instance Read CheckSignupQueueResult
instance Eq CheckSignupQueueResult
instance Ord CheckSignupQueueResult
instance Data.Aeson.FromJSON CheckSignupQueueResult

instance Data.Aeson.ToJSON CheckSignupQueueResult

checkSignupQueueResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => CheckSignupQueueResult -> kv
