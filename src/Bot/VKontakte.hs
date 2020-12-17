{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Bot.VKontakte where

import           Control.Applicative        ((<|>))
import           Control.Monad              (replicateM_)
import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString)
import           Data.Function              ((&))
import           Data.Functor               ((<&>))
import           Data.Map.Strict            (fromList, insert)
import           Data.Text                  as T (Text, intercalate)

import           Bot.Types
import           Logging
import           Utils
import           VKontakte.API              (Keyboard (..), Method (..),
                                             mkButton)
import qualified VKontakte.API              as VKontakte (sendMethod)
import           VKontakte.Types


apiVersion :: String
apiVersion = "5.126"

data VKontakteEnv =
  VKontakteEnv
    { token   :: String
    , groupId :: String
    , server  :: String
    , key     :: String
    , ts      :: String
    } deriving Show


instance Bot VKontakteEnv where
  type BotIncome VKontakteEnv = Response
  type BotUpdate VKontakteEnv = Update

  getIncome Model{..} = do
    let method = UpdatesGet
          { uServer = server mPlatformEnv
          , uKey = key mPlatformEnv
          , uAct = "a_check"
          , uWait = 25
          , uTs = ts mPlatformEnv
          }
    logInfo' mLogLevel "Send request to VKontakte server and wait for response."
    eResponse <- VKontakte.sendMethod (logDebug mLogLevel) method
    case eResponse of
      Left msg       -> pure $ Left msg
      Right response -> do
        logInfo' mLogLevel "Bot got response."
        logDebug mLogLevel response
        pure $ eitherDecode response

  handleUpdate model update =
    case oMessage $ uObject update of
      Just msg -> handleMessage model msg
      _        ->
        if uType update == "message_reply"
           then do
             logInfo' (mLogLevel model) "No messages from user."
             pure model
           else do
             logWarning' (mLogLevel model) "There is no message in Update."
             pure model

  updateModel model@Model{mPlatformEnv=mPlatformEnv} response =
    case rTs response of
      Nothing        -> model
      Just timestamp ->
        let mPlatformEnv' = mPlatformEnv{ts = timestamp}
        in model{mPlatformEnv=mPlatformEnv'}

  extractUpdates income _ = rUpdates income


handleMessage :: Model VKontakteEnv -> Message -> IO (Model VKontakteEnv)
handleMessage model@Model{..} Message{..} =
  case mText of
    "/1" -> setRepeatsNumber 1 model
    "/2" -> setRepeatsNumber 2 model
    "/3" -> setRepeatsNumber 3 model
    "/4" -> setRepeatsNumber 4 model
    "/5" -> setRepeatsNumber 5 model
    "/help"   -> sendMethod messagesSend{mMessage=Just bHelpMessage}
    "/repeat" -> sendMethod messagesSend{mMessage=Just repeatMessage
                                        ,mKeyboard=Just $ encode numKeyboard}
    _         -> do
      let attachment =
              fmap toAttachmentFormat mAttachments
              & filter (/=Nothing)
              & sequence
             <&> T.intercalate ","
      logInfo mLogLevel ("Message of user '" <> gshow mFromId <> "' would be echoed " <> nTimes echoNumber <> ".")
      sendEcho echoNumber messagesSend{mMessage=Just mText
                                      ,mStickerId=getStickerId mAttachments
                                      ,mAttachment=attachment}
  where
    BotSettings{..}  = mBotSettings
    VKontakteEnv{..} = mPlatformEnv

    getStickerId [] = Nothing
    getStickerId l  = sStickerId <$> (aSticker $ head l)

    repeatMessage = "Current number of repeats = " <> gshow echoNumber <> ".\n" <> bRepeatMessage

    numKeyboard =
      Keyboard
        { kOneTime = True
        , kButtons = [fmap mkButton (gshow <$> ([1..5] :: [Int]))]
        }

    (echoNumber, mUsersSettings') = lookupInsert mFromId bNumberOfRepeats mUsersSettings

    setRepeatsNumber number model' =
      logInfo' mLogLevel ("Number of repeats for user: " <> gshow mFromId <> " changed to " <> gshow echoNumber <> ".")
      >> pure model'{mUsersSettings=insert mFromId number mUsersSettings}


    sendMethod :: Method -> IO (Model VKontakteEnv)
    sendMethod  m = do
      logInfo' mLogLevel "Send request with 'messages.send' method to reply to user's command."
      result <- VKontakte.sendMethod (logDebug mLogLevel) m
      case result of
        Left msg   -> logWarning mLogLevel msg
        Right resp -> logInfo' mLogLevel "Reply to command has been sent."
                   >> logDebug mLogLevel ("\n" <> resp)

      pure model{mUsersSettings=mUsersSettings'}

    sendEcho :: Int -> Method -> IO (Model VKontakteEnv)
    sendEcho num m = do
      logInfo mLogLevel ("Send request with 'messages.send' method " <> nTimes echoNumber <> " to echo user's message.")
      replicateM_ num $ do
        result <- VKontakte.sendMethod (logDebug mLogLevel) m
        case result of
           Left msg   -> logWarning mLogLevel msg
           Right resp -> logInfo' mLogLevel "Message has been echoed."
                      >> logDebug mLogLevel ("\n" <> resp)

      pure model{mUsersSettings=mUsersSettings'}

    messagesSend =
      MessagesSend
        { mAccessToken = token
        , mGroupId = groupId
        , mUserId  = mFromId
        , mRandomId = mRandomId
        , mMessage = Nothing
        , mAttachment = Nothing
        , mStickerId = Nothing
        , mKeyboard = Nothing
        , mV = apiVersion
        }


toAttachmentFormat :: Attachment -> Maybe Text
toAttachmentFormat Attachment{..} =
        aPhoto *> fmap helper aPhoto
    <|> aVideo *> fmap helper aVideo
    <|> aDoc   *> fmap helper aDoc
    <|> aAudio *> fmap helper aAudio
  where
    helper Media{..} = aType <> gshow mOwnerId <> "_" <> gshow mId <> maybe "" ("_" <>) mAccessKey


getModel :: Config -> IO (Either L8.ByteString (Model VKontakteEnv))
getModel Config{cGroupId = Nothing} = pure $ Left "group_id is required for VKontakte."
getModel Config{cGroupId = Just groupId,..} = do
    rawResponse <- VKontakte.sendMethod (logDebug cLogLevel) $
                      GetLongPollServer
                          { gAccessToken = cToken
                          , gGroupId = groupId
                          , gV = apiVersion
                          }
    logDebug cLogLevel rawResponse
    pure $ do
      rawResponse' <- rawResponse
      resp <- eitherDecode rawResponse'
      skt <- maybe (Left "Failed on getting Update.") Right (rResponse resp)
      pure $ model skt
  where
    model serverKeyTs = Model
              { mBotSettings   = cBotSettings
              , mPlatformEnv   = platformEnv serverKeyTs
              , mUsersSettings = fromList []
              , mLogLevel      = cLogLevel
              }
    platformEnv ServerKeyTs{..} =
      VKontakteEnv
        { token   = cToken
        , groupId = groupId
        , server  = sServer
        , key     = sKey
        , ts      = sTs
        }

