module Chat.Bsky.Convo.Defs where

import qualified Data.Aeson

data ConvoView

instance Show ConvoView
instance Read ConvoView
instance Eq ConvoView
instance Ord ConvoView
instance Data.Aeson.FromJSON ConvoView

instance Data.Aeson.ToJSON ConvoView

convoView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => ConvoView -> kv
data DeletedMessageView

instance Show DeletedMessageView
instance Read DeletedMessageView
instance Eq DeletedMessageView
instance Ord DeletedMessageView
instance Data.Aeson.FromJSON DeletedMessageView

instance Data.Aeson.ToJSON DeletedMessageView

deletedMessageView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => DeletedMessageView -> kv
data LogBeginConvo

instance Show LogBeginConvo
instance Read LogBeginConvo
instance Eq LogBeginConvo
instance Ord LogBeginConvo
instance Data.Aeson.FromJSON LogBeginConvo

instance Data.Aeson.ToJSON LogBeginConvo

logBeginConvo'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => LogBeginConvo -> kv
data LogCreateMessage

instance Show LogCreateMessage
instance Read LogCreateMessage
instance Eq LogCreateMessage
instance Ord LogCreateMessage
instance Data.Aeson.FromJSON LogCreateMessage

instance Data.Aeson.ToJSON LogCreateMessage

logCreateMessage'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => LogCreateMessage -> kv
data LogDeleteMessage

instance Show LogDeleteMessage
instance Read LogDeleteMessage
instance Eq LogDeleteMessage
instance Ord LogDeleteMessage
instance Data.Aeson.FromJSON LogDeleteMessage

instance Data.Aeson.ToJSON LogDeleteMessage

logDeleteMessage'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => LogDeleteMessage -> kv
data LogLeaveConvo

instance Show LogLeaveConvo
instance Read LogLeaveConvo
instance Eq LogLeaveConvo
instance Ord LogLeaveConvo
instance Data.Aeson.FromJSON LogLeaveConvo

instance Data.Aeson.ToJSON LogLeaveConvo

logLeaveConvo'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => LogLeaveConvo -> kv
data MessageInput

instance Show MessageInput
instance Read MessageInput
instance Eq MessageInput
instance Ord MessageInput
instance Data.Aeson.FromJSON MessageInput

instance Data.Aeson.ToJSON MessageInput

messageInput'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => MessageInput -> kv
data MessageRef

instance Show MessageRef
instance Read MessageRef
instance Eq MessageRef
instance Ord MessageRef
instance Data.Aeson.FromJSON MessageRef

instance Data.Aeson.ToJSON MessageRef

messageRef'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => MessageRef -> kv
data MessageView

instance Show MessageView
instance Read MessageView
instance Eq MessageView
instance Ord MessageView
instance Data.Aeson.FromJSON MessageView

instance Data.Aeson.ToJSON MessageView

messageView'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => MessageView -> kv
data MessageViewSender

instance Show MessageViewSender
instance Read MessageViewSender
instance Eq MessageViewSender
instance Ord MessageViewSender
instance Data.Aeson.FromJSON MessageViewSender

instance Data.Aeson.ToJSON MessageViewSender

messageViewSender'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => MessageViewSender -> kv
data ConvoViewLastMessageKind

instance Show ConvoViewLastMessageKind
instance Read ConvoViewLastMessageKind
instance Eq ConvoViewLastMessageKind
instance Ord ConvoViewLastMessageKind
instance Data.Aeson.FromJSON ConvoViewLastMessageKind

instance Data.Aeson.ToJSON ConvoViewLastMessageKind

data LogCreateMessageMessageKind

instance Show LogCreateMessageMessageKind
instance Read LogCreateMessageMessageKind
instance Eq LogCreateMessageMessageKind
instance Ord LogCreateMessageMessageKind
instance Data.Aeson.FromJSON LogCreateMessageMessageKind

instance Data.Aeson.ToJSON LogCreateMessageMessageKind

data LogDeleteMessageMessageKind

instance Show LogDeleteMessageMessageKind
instance Read LogDeleteMessageMessageKind
instance Eq LogDeleteMessageMessageKind
instance Ord LogDeleteMessageMessageKind
instance Data.Aeson.FromJSON LogDeleteMessageMessageKind

instance Data.Aeson.ToJSON LogDeleteMessageMessageKind

data MessageInputEmbedKind

instance Show MessageInputEmbedKind
instance Read MessageInputEmbedKind
instance Eq MessageInputEmbedKind
instance Ord MessageInputEmbedKind
instance Data.Aeson.FromJSON MessageInputEmbedKind

instance Data.Aeson.ToJSON MessageInputEmbedKind

data MessageViewEmbedKind

instance Show MessageViewEmbedKind
instance Read MessageViewEmbedKind
instance Eq MessageViewEmbedKind
instance Ord MessageViewEmbedKind
instance Data.Aeson.FromJSON MessageViewEmbedKind

instance Data.Aeson.ToJSON MessageViewEmbedKind
