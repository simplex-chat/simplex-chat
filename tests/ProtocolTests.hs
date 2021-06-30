{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module ProtocolTests where

import Data.ByteString.Char8 (ByteString)
import Simplex.Chat.Protocol
import Simplex.Messaging.Parsers (parseAll)
import Test.Hspec

protocolTests :: Spec
protocolTests = do
  parseChatMessageTest

(#==) :: ByteString -> RawChatMessage -> Expectation
s #== msg = parseAll rawChatMessageP s `shouldBe` Right msg

parseChatMessageTest :: Spec
parseChatMessageTest = describe "Raw chat message format" $ do
  it "should parse raw chat messages" $ do
    "5 x.grp.mem.leave   " #== RawChatMessage (Just 5) (SimplexChatMsgEvent ["grp", "mem", "leave"]) [] []
    "6 x.msg.del 3  " #== RawChatMessage (Just 6) (SimplexChatMsgEvent ["msg", "del"]) ["3"] []
    "7 x.msg.new c.text c.text:11 hello there "
      #== RawChatMessage
        (Just 7)
        (SimplexChatMsgEvent ["msg", "new"])
        ["c.text"]
        [MsgBodyContent (ChannelContentType "text") Nothing $ MBFull (MsgData "hello there")]
    "8 x.msg.new c.image x.dag:16,c.text:7,i.image/jpg:64:MDEyMzQ1Njc=,i.image/png:4000:MDEyMzQ1Njc= 0123456789012345 picture abcdef"
      #== RawChatMessage
        (Just 8)
        (SimplexChatMsgEvent ["msg", "new"])
        ["c.image"]
        [ MsgBodyContent SimplexDAG Nothing $ MBFull (MsgData "0123456789012345"),
          MsgBodyContent (ChannelContentType "text") Nothing $ MBFull (MsgData "picture"),
          MsgBodyContent (MimeContentType "image/jpg") (Just "01234567") $ MBPartial 64 (MsgData "abcdef"),
          MsgBodyContent (MimeContentType "image/png") (Just "01234567") $ MBEmpty 4000
        ]
    "#8.1 abcdef" #== RawChatMsgContinuation 8 1 "abcdef"
