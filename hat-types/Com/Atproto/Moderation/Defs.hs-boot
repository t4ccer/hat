module Com.Atproto.Moderation.Defs where

import qualified Data.Aeson

data ReasonType

instance Show ReasonType
instance Read ReasonType
instance Eq ReasonType
instance Ord ReasonType
instance Data.Aeson.FromJSON ReasonType

instance Data.Aeson.ToJSON ReasonType
