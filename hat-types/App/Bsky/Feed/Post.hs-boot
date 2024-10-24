module App.Bsky.Feed.Post where

import qualified Data.Aeson

data Entity

instance Show Entity
instance Read Entity
instance Eq Entity
instance Ord Entity
instance Data.Aeson.FromJSON Entity

instance Data.Aeson.ToJSON Entity

entity'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Entity -> kv
data Post

instance Show Post
instance Read Post
instance Eq Post
instance Ord Post
instance Data.Aeson.FromJSON Post

instance Data.Aeson.ToJSON Post

post'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => Post -> kv
data ReplyRef

instance Show ReplyRef
instance Read ReplyRef
instance Eq ReplyRef
instance Ord ReplyRef
instance Data.Aeson.FromJSON ReplyRef

instance Data.Aeson.ToJSON ReplyRef

replyRef'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ReplyRef -> kv
data TextSlice

instance Show TextSlice
instance Read TextSlice
instance Eq TextSlice
instance Ord TextSlice
instance Data.Aeson.FromJSON TextSlice

instance Data.Aeson.ToJSON TextSlice

textSlice'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => TextSlice -> kv
data PostEmbedKind

instance Show PostEmbedKind
instance Read PostEmbedKind
instance Eq PostEmbedKind
instance Ord PostEmbedKind
instance Data.Aeson.FromJSON PostEmbedKind

instance Data.Aeson.ToJSON PostEmbedKind

data PostLabelsKind

instance Show PostLabelsKind
instance Read PostLabelsKind
instance Eq PostLabelsKind
instance Ord PostLabelsKind
instance Data.Aeson.FromJSON PostLabelsKind

instance Data.Aeson.ToJSON PostLabelsKind
