module VKontakte.API where


data ServerKeyTs =
  ServerKeyTs
    { server :: String
    , key    :: String
    , ts     :: String
    }

getLongPollServer :: String -> String -> IO (Either String ServerKeyTs)
getLongPollServer token groupId = undefined

