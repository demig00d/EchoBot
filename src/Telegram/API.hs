module Telegram.API where


getMe :: String -> IO (Either String ())
getMe _ = pure $ Right ()

