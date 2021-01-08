{-# LANGUAGE OverloadedStrings #-}
module VKontakteSpec where

import qualified Data.ByteString.Char8 as S8 (putStrLn)
import           Data.Functor.Identity
import           Data.Map.Strict       (empty)
import           Data.Text             (Text)
import           Test.Hspec

import           Bot.Types
import           Bot.VKontakte
import           Logging
import           Requests
import           VKontakte.API
import           VKontakte.Types


model :: Model VKontakteEnv
model =
  Model
      { botSettings =
         BotSettings
           { bHelpMessage = "I am bot that can echo your messages."
           , bRepeatMessage = "Choose number of repeats:"
           , bNumberOfRepeats = 2
           }
      , platformEnv =
         VKontakteEnv
           { token   = "<token>"
           , groupId = "923456789"
           , server  = "https://server.com"
           , key     = "46567asdfgh"
           , ts      = "3"
           }
      , usersSettings = empty
      , logLevel = Debug
      }


getIncomeQuery :: Requests.Handler
getIncomeQuery =
  Requests.Handler
    { url = "https://server.com"
    , body = "key=46567asdfgh&act=a_check&wait=25&ts=3"
    , headers=[("Content-Type","application/x-www-form-urlencoded")]
    , logger = S8.putStrLn
    }

textUpdate :: Text -> Update
textUpdate text = Update
    { uObject =
        Object
          { oMessage =
             Just $
               Message
                 { mFromId = 12345678
                 , mText = text
                 , mRandomId = 0
                 , mAttachments = []
                 }
          }
          , uType = "message_new"
    }


stickerUpdate :: Update
stickerUpdate = Update
    { uObject =
      Object
        { oMessage =
           Just $
             Message
               { mFromId = 12345678
               , mText = ""
               , mRandomId = 0
               , mAttachments =
                    [Attachment
                      { aType = "sticker"
                      , aSticker = Just $ Sticker 9046
                      , aPhoto = Nothing
                      , aVideo = Nothing
                      , aAudio = Nothing
                      , aDoc = Nothing
                      }
                    ]
               }
        }
  , uType = "message_new"
  }


attachmentsUpdate :: Update
attachmentsUpdate = Update
  { uObject =
      Object
        { oMessage =
            Just $
              Message
                { mFromId = 12345678
                , mText = ""
                , mRandomId = 0
                , mAttachments =
                    [Attachment
                      { aType = "photo"
                      , aSticker = Nothing
                      , aPhoto =
                        Just $
                          Media
                            { mAccessKey = Just "123ab4567c89876543"
                            , mId = 111111143
                            , mOwnerId = 12345678
                            }
                      , aVideo = Nothing
                      , aAudio = Nothing
                      , aDoc   = Nothing
                      }
                    ,Attachment
                      { aType = "audio"
                      , aSticker = Nothing
                      , aPhoto =
                        Just $
                          Media
                            { mAccessKey = Nothing
                            , mId = 111111144
                            , mOwnerId = 12345678
                            }
                      , aVideo = Nothing
                      , aAudio = Nothing
                      , aDoc   = Nothing
                      }
                    ]
                }
        }
  , uType = "message_new"
  }


replyToHelp :: Bot.VKontakte.Action
replyToHelp = ReplyToCommand $
  MessagesSend
    { mAccessToken = "<token>"
    , mGroupId = "923456789"
    , mUserId = 12345678
    , mRandomId = 0
    , mMessage = Just "I am bot that can echo your messages."
    , mAttachment = Nothing
    , mStickerId = Nothing
    , mKeyboard = Nothing
    , mV = "5.126"
    }

replyToRepeat :: Bot.VKontakte.Action
replyToRepeat = ReplyToCommand $
  MessagesSend
    { mAccessToken = "<token>"
    , mGroupId = "923456789"
    , mUserId = 12345678
    , mRandomId = 0
    , mMessage = Just "Current number of repeats = 2.\nChoose number of repeats:"
    , mAttachment = Nothing
    , mStickerId = Nothing
    , mKeyboard = Just "{\"one_time\":true,\"buttons\":[[{\"action\":{\"type\":\"text\",\"payload\":{\"button\":\"/1\"},\"label\":\"/1\"}},{\"action\":{\"type\":\"text\",\"payload\":{\"button\":\"/2\"},\"label\":\"/2\"}},{\"action\":{\"type\":\"text\",\"payload\":{\"button\":\"/3\"},\"label\":\"/3\"}},{\"action\":{\"type\":\"text\",\"payload\":{\"button\":\"/4\"},\"label\":\"/4\"}},{\"action\":{\"type\":\"text\",\"payload\":{\"button\":\"/5\"},\"label\":\"/5\"}}]]}"
    , mV = "5.126"
    }

echoText :: Text -> Bot.VKontakte.Action
echoText text = SendEcho 2
      (MessagesSend
          {mAccessToken = "<token>"
          , mGroupId = "923456789"
          , mUserId = 12345678
          , mRandomId = 0
          , mMessage = Just text
          , mAttachment = Just ""
          , mStickerId = Nothing
          , mKeyboard = Nothing
          , mV = "5.126"
          }
      )

echoSticker :: Bot.VKontakte.Action
echoSticker = SendEcho 2 $
  MessagesSend
    { mAccessToken = "<token>"
    , mGroupId = "923456789"
    , mUserId = 12345678
    , mRandomId = 0
    , mMessage = Just ""
    , mAttachment = Just ""
    , mStickerId = Just 9046
    , mKeyboard = Nothing
    , mV = "5.126"
    }

echoAttachments :: Bot.VKontakte.Action
echoAttachments = SendEcho 2 $
  MessagesSend
    { mAccessToken = "<token>"
    , mGroupId = "923456789"
    , mUserId = 12345678
    , mRandomId = 0
    , mMessage = Just ""
    , mAttachment = Just "photo12345678_111111143_123ab4567c89876543,audio12345678_111111144"
    , mStickerId = Nothing
    , mKeyboard = Nothing
    , mV = "5.126"
    }


spec :: Spec
spec = do
  describe "VKontakte methods:" $ do
    it "form request to get updates" $
      encodeGetIncome model `shouldBe` getIncomeQuery

    it "handle update with '/help' command" $
      runIdentity (handleUpdate model (textUpdate "/help")) `shouldBe` replyToHelp

    it "handle update with '/repeat' command" $
      runIdentity (handleUpdate model (textUpdate "/repeat")) `shouldBe` replyToRepeat

    it "handle update with text" $
      let text = "some text"
      in runIdentity (handleUpdate model (textUpdate text)) `shouldBe` echoText text

    it "handle update with sticker" $
      runIdentity (handleUpdate model stickerUpdate) `shouldBe` echoSticker

    it "handle update with attachments" $
      runIdentity (handleUpdate model attachmentsUpdate) `shouldBe` echoAttachments
