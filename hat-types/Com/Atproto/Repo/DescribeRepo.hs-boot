module Com.Atproto.Repo.DescribeRepo where

import qualified Data.Aeson

data DescribeRepoResult

instance Show DescribeRepoResult
instance Read DescribeRepoResult
instance Eq DescribeRepoResult
instance Ord DescribeRepoResult
instance Data.Aeson.FromJSON DescribeRepoResult

instance Data.Aeson.ToJSON DescribeRepoResult

describeRepoResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => DescribeRepoResult -> kv
