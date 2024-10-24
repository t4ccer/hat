module Com.Atproto.Server.DescribeServer where

import qualified Data.Aeson

data Contact

instance Show Contact
instance Read Contact
instance Eq Contact
instance Ord Contact
instance Data.Aeson.FromJSON Contact

instance Data.Aeson.ToJSON Contact

contact'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Contact -> kv
data Links

instance Show Links
instance Read Links
instance Eq Links
instance Ord Links
instance Data.Aeson.FromJSON Links

instance Data.Aeson.ToJSON Links

links'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Links -> kv
data DescribeServerResult

instance Show DescribeServerResult
instance Read DescribeServerResult
instance Eq DescribeServerResult
instance Ord DescribeServerResult
instance Data.Aeson.FromJSON DescribeServerResult

instance Data.Aeson.ToJSON DescribeServerResult

describeServerResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => DescribeServerResult -> kv
