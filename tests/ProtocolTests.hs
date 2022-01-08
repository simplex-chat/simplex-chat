{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module ProtocolTests where

import Data.ByteString.Char8 (ByteString)
import Simplex.Chat.Protocol.Legacy
import "simplexmq" Simplex.Messaging.Parsers (parseAll)
import Test.Hspec

protocolTests :: Spec
protocolTests = do
  parseChatMessageTest

(#==) :: ByteString -> RawChatMessage -> Expectation
s #== msg = parseAll rawChatMessageP s `shouldBe` Right msg

parseChatMessageTest :: Spec
parseChatMessageTest = describe "Raw chat message format" $ do
  it "no parameters and content" $
    "5 x.grp.mem.leave   " #== RawChatMessage "x.grp.mem.leave" [] []
  it "one parameter, no content" $
    "6 x.msg.del 3  " #== RawChatMessage "x.msg.del" ["3"] []
  it "with content that fits the message" $
    "7 x.msg.new c.text x.text:11 hello there "
      #== RawChatMessage
        "x.msg.new"
        ["c.text"]
        [RawMsgBodyContent (RawContentType "x" "text") "hello there"]
  it "with DAG reference and partial content" $
    "8 x.msg.new c.image x.dag:16,x.text:7,m.image/jpg:6 0123456789012345 picture abcdef "
      #== RawChatMessage
        "x.msg.new"
        ["c.image"]
        [ RawMsgBodyContent (RawContentType "x" "dag") "0123456789012345",
          RawMsgBodyContent (RawContentType "x" "text") "picture",
          RawMsgBodyContent (RawContentType "m" "image/jpg") "abcdef"
        ]
  it "without message id" $
    " x.grp.mem.inv 23456,123 x.json:46 {\"contactRef\":\"john\",\"displayName\":\"John Doe\"} "
      #== RawChatMessage
        "x.grp.mem.inv"
        ["23456", "123"]
        [RawMsgBodyContent (RawContentType "x" "json") "{\"contactRef\":\"john\",\"displayName\":\"John Doe\"}"]
