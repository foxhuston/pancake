module Network.Irc.Configuration (Configuration 
    (Configure, nick, user, privMessageHandler))

where

import Network.Irc.Types

data Configuration = Configure {
    nick :: String,
    user :: String,
    privMessageHandler :: (Channel -> Nick -> Message -> Maybe (Channel, Message))
}
