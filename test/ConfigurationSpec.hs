{-# LANGUAGE OverloadedStrings #-}
module ConfigurationSpec where

import qualified Data.ByteString.Char8 as S8 (ByteString)
import           Test.Hspec

import           Common                (telegramConfig, vkontakteConfig)
import           Configuration


rawTelegramConfig :: S8.ByteString
rawTelegramConfig = "{\
  \\"bot_settings\":\
    \{\
      \\"help_message\": \"I am bot that can echo your messages.\",\
      \\"repeat_message\": \"Choose number of repeats:\",\
      \\"number_of_repeats\": 2\
    \},\
  \\"platform_name\":  \"Telegram\",\
  \\"token\": \"<token>\",\
  \\"log_level\": \"Debug\"\
\}"

rawVKontakteConfig :: S8.ByteString
rawVKontakteConfig = "{\
  \\"bot_settings\":\
    \{\
      \\"help_message\": \"I am bot that can echo your messages.\",\
      \\"repeat_message\": \"Choose number of repeats:\",\
      \\"number_of_repeats\": 2\
    \},\
  \\"platform_name\":  \"VKontakte\",\
  \\"token\": \"<token>\",\
  \\"group_id\": \"123456\",\
  \\"log_level\": \"Debug\"\
\}"

rawWrongTelegramConfig :: S8.ByteString
rawWrongTelegramConfig = "{\
  \\"bot_settings\":\
    \{\
      \\"help_message\": \"I am bot that can echo your messages.\",\
      \\"repeat_message\": \"Choose number of repeats:\",\
      \\"number_of_repeats\": 2\
    \},\
  \\"platform_name\":  \"Telegram\",\
  \\"log_level\": \"Debug\"\
\}"


rawWrongVKontakteConfig :: S8.ByteString
rawWrongVKontakteConfig = "{\
  \\"bot_settings\":\
    \{\
      \\"help_message\": \"I am bot that can echo your messages.\",\
      \\"repeat_message\": \"Choose number of repeats:\",\
      \\"number_of_repeats\": 2\
    \},\
  \\"platform_name\":  \"VKontakte\",\
  \\"token\": \"<token>\",\
  \\"log_level\": \"Debug\"\
\}"


spec :: Spec
spec = do
  describe "Telegram configuration parsing:" $ do
    it "parse correct Telegram config" $
      parseConfig (Right rawTelegramConfig) `shouldBe` Right telegramConfig

    it "parse incorrect Telegram config" $
      parseConfig (Right rawWrongTelegramConfig) `shouldBe` Left "Error in $: When parsing the record Config of type Bot.Types.Config the key token was not present."


  describe "VKontakte configuration parsing:" $ do
    it "parse correct VKontakte config" $
      parseConfig (Right rawVKontakteConfig) `shouldBe` Right vkontakteConfig

    it "parse incorrect VKontakte config" $
      parseConfig (Right rawWrongVKontakteConfig) `shouldBe` Left "group_id is required for VKontakte."
