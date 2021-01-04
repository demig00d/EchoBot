{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Bot.VKontakte where

import           Control.Applicative        ((<|>))
import           Control.Monad              (replicateM_)
import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString)
import           Data.Function              ((&))
import           Data.Functor               ((<&>))
import           Data.Map.Strict            (empty, findWithDefault, insert)
import           Data.Text                  as T (Text, intercalate)

import           Bot.Types
import           Logging
import           Requests
import           Utils
import           VKontakte.API              (Keyboard (..), Method (..),
                                             mkButton)
import qualified VKontakte.API              as VKontakte (encodeRequest)
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

  getIncome Model{platformEnv, logLevel} = do
    let method = UpdatesGet
          { uServer = server platformEnv
          , uKey = key platformEnv
          , uAct = "a_check"
          , uWait = 25
          , uTs = ts platformEnv
          }
    logInfo' logLevel "Send request to VKontakte server and wait for response."
    let request = VKontakte.encodeRequest (logDebug logLevel) method
    eResponse <- sendPost request
    case eResponse of
      Left msg       -> pure $ Left msg
      Right response -> do
        logInfo' logLevel "Bot got response."
        logDebug logLevel response
        pure $ eitherDecode response

  handleUpdate model update =
    case oMessage $ uObject update of
      Just msg -> handleMessage model msg
      _        ->
        if uType update == "message_reply"
           then do
             logInfo' (logLevel model) "No messages from user."
             pure model
           else do
             logWarning' (logLevel model) "There is no message in Update."
             pure model

  updateModel model@Model{platformEnv=platformEnv} response =
    case rTs response of
      Nothing        -> model
      Just timestamp ->
        let platformEnv' = platformEnv{ts = timestamp}
        in model{platformEnv=platformEnv'}

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
      logInfo logLevel ("Message of user '" <> gshow mFromId <> "' would be echoed " <> nTimes echoNumber <> ".")
      sendEcho echoNumber messagesSend{mMessage=Just mText
                                      ,mStickerId=getStickerId mAttachments
                                      ,mAttachment=attachment}
  where
    BotSettings{..}  = botSettings
    VKontakteEnv{..} = platformEnv

    getStickerId [] = Nothing
    getStickerId l  = sStickerId <$> aSticker (head l)

    repeatMessage = "Current number of repeats = " <> gshow echoNumber <> ".\n" <> bRepeatMessage

    numKeyboard =
      Keyboard
        { kOneTime = True
        , kButtons = [fmap mkButton (gshow <$> ([1..5] :: [Int]))]
        }

    echoNumber = findWithDefault bNumberOfRepeats mFromId usersSettings

    setRepeatsNumber number model' =
      logInfo' logLevel ("Number of repeats for user: " <> gshow mFromId <> " changed to " <> gshow echoNumber <> ".")
      >> pure model'{usersSettings=insert mFromId number usersSettings}


    sendMethod :: Method -> IO (Model VKontakteEnv)
    sendMethod  m = do
      logInfo' logLevel "Send request with 'messages.send' method to reply to user's command."
      let request = VKontakte.encodeRequest (logDebug logLevel) m
      result <- sendPost request
      case result of
        Left msg   -> logWarning logLevel msg
        Right resp -> logInfo' logLevel "Reply to command has been sent."
                   >> logDebug logLevel ("\n" <> resp)

      pure model

    sendEcho :: Int -> Method -> IO (Model VKontakteEnv)
    sendEcho num m = do
      logInfo logLevel ("Send request with 'messages.send' method " <> nTimes echoNumber <> " to echo user's message.")
      replicateM_ num $ do
        let request = VKontakte.encodeRequest (logDebug logLevel) m
        result <- sendPost request
        case result of
           Left msg   -> logWarning logLevel msg
           Right resp -> logInfo' logLevel "Message has been echoed."
                      >> logDebug logLevel ("\n" <> resp)

      pure model

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
getModel Config{cGroupId = Just groupId, cToken, cLogLevel, cBotSettings} = do
  let request = VKontakte.encodeRequest (logDebug cLogLevel) $
                    GroupsGetLongPollServer
                        { gAccessToken = cToken
                        , gGroupId = groupId
                        , gV = apiVersion
                        }
  rawResponse <- sendPost request

  logDebug cLogLevel rawResponse
  pure $ do
    rawResponse' <- rawResponse
    resp <- eitherDecode rawResponse'
    skt <- maybe (Left "Failed on getting Update.") Right (rResponse resp)
    pure $ model skt
  where
    model serverKeyTs = Model
              { botSettings   = cBotSettings
              , platformEnv   = platformEnv serverKeyTs
              , usersSettings = empty
              , logLevel      = cLogLevel
              }
    platformEnv ServerKeyTs{..} =
      VKontakteEnv
        { token   = cToken
        , groupId = groupId
        , server  = sServer
        , key     = sKey
        , ts      = sTs
        }

