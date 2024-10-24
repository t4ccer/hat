module Com.Atproto.Admin.GetSubjectStatus where

import qualified Data.Aeson

data GetSubjectStatusResult

instance Show GetSubjectStatusResult
instance Read GetSubjectStatusResult
instance Eq GetSubjectStatusResult
instance Ord GetSubjectStatusResult
instance Data.Aeson.FromJSON GetSubjectStatusResult

instance Data.Aeson.ToJSON GetSubjectStatusResult

getSubjectStatusResult'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => GetSubjectStatusResult -> kv
data GetSubjectStatusResultSubjectKind

instance Show GetSubjectStatusResultSubjectKind
instance Read GetSubjectStatusResultSubjectKind
instance Eq GetSubjectStatusResultSubjectKind
instance Ord GetSubjectStatusResultSubjectKind
instance Data.Aeson.FromJSON GetSubjectStatusResultSubjectKind

instance Data.Aeson.ToJSON GetSubjectStatusResultSubjectKind
