{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.NamesLinksTests (namesLinksTests) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Simplex.Messaging.Transport.Client (TransportHost (..))
import StoreService.Names.Links
import Test.Hspec

namesLinksTests :: Spec
namesLinksTests = describe "StoreService names: links" $ do
  describe "kinds" $ do
    it "accepts the page's short links of each kind" $ do
      linkKindOf pageContact `shouldBe` Just LKContact
      linkKindOf pageChannel `shouldBe` Just LKChannel
      linkKindOf "https://smp9.simplex.im/c#Rk27xN4pDe9Tz1Wb3Yc5Vd7Xf9Hg2Jk4Lm" `shouldBe` Just LKChannel
      linkKindOf "https://smp8.simplex.im/a#x" `shouldBe` Just LKContact
      linkKindOf "simplex:/a#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw?h=smp6.simplex.im" `shouldBe` Just LKContact
      linkKindOf "https://localhost/a#4AkRDmhf64tdRlN406g8lJRg5OCmhD6ynIhi6glOcCM?p=7001&c=LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI" `shouldBe` Just LKContact
      linkKindOf "https://1.2.3.4/c#MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY" `shouldBe` Just LKChannel
    it "refuses full links, two links joined, other link types and anything else" $ do
      mapM_
        refused
        [ "https://simplex.chat/contact#/?v=2&smp=smp%3A%2F%2Fabc",
          pageContact <> ";" <> pageContact,
          "hello",
          "",
          "https://smp8.simplex.im/g#MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY",
          "https://smp8.simplex.im/i#MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3/MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY",
          "simplex:/g#MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY?h=smp.simplex.im",
          "http://smp8.simplex.im/a#Y8m3qT0vLw2Ab7cD9eF1gH3jK5mN7pQ9rS",
          "https://SMP8.simplex.im/a#Y8m3qT0vLw2Ab7cD9eF1gH3jK5mN7pQ9rS",
          "https://smp8.simplex.im/a/#Y8m3qT0vLw2Ab7cD9eF1gH3jK5mN7pQ9rS",
          "https://smp8.simplex.im/a#",
          "https:///a#Y8m3qT0vLw2Ab7cD9eF1gH3jK5mN7pQ9rS",
          "simplex:/a#Y8m3qT0vLw2Ab7cD9eF1gH3jK5mN7pQ9rS=",
          " " <> pageContact,
          pageContact <> "\n",
          pageContact <> " ",
          pageContact <> "?x=é"
        ]
    it "accepts a link with several hosts, as simplexmq writes it" $ do
      linkKindOf multiHostContact `shouldBe` Just LKContact
      linkKindOf multiHostChannel `shouldBe` Just LKChannel
    it "refuses any other escape in the query" $
      mapM_
        refused
        [ "simplex:/a#MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY?h=smp.simplex.im%2Fx",
          "simplex:/a#MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY?h=smp.simplex.im%2cx.onion",
          "simplex:/a#MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY?h=smp.simplex.im%"
        ]
    it "accepts a link of exactly the bound and refuses one byte over it" $ do
      let atBound = padded linkMaxBytes
      T.length atBound `shouldBe` linkMaxBytes
      linkKindOf atBound `shouldBe` Just LKContact
      linkKindOf (padded $ linkMaxBytes + 1) `shouldBe` Nothing
  describe "hosts" $ do
    it "reads the authority of an https link" $
      linkHosts presetContact `shouldBe` [THDomainName "smp6.simplex.im"]
    it "reads an IP literal authority" $
      linkHosts "https://1.2.3.4/c#MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY" `shouldBe` [THIPv4 (1, 2, 3, 4)]
    it "reads the authority and the extra hosts of an https link" $
      linkHosts "https://smp.simplex.im/a#MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY?h=jjbyvoemxysm7qxap7m5d5m35jzv5qq6gnlv7s4rsn7tdwwmuqciwpid.onion&p=5223&c=1234-w"
        `shouldBe` [THDomainName "smp.simplex.im", THOnionHost onionHost]
    it "reads the one server host of a simplex link" $
      linkHosts "simplex:/a#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw?h=smp6.simplex.im" `shouldBe` [THDomainName "smp6.simplex.im"]
    it "reads every server host of a simplex link" $ do
      linkHosts multiHostContact `shouldBe` [THDomainName "smp.simplex.im", THOnionHost onionHost]
      linkHosts multiHostChannel `shouldBe` [THDomainName "smp.simplex.im", THOnionHost onionHost]
    it "reads the hosts of a channel link" $
      linkHosts "simplex:/c#AQIDBAUGBwgBAgMEBQYHCAECAwQFBgcIAQIDBAUGBwg?h=smp.simplex.im&p=5223&c=1234-w" `shouldBe` [THDomainName "smp.simplex.im"]
    it "has no hosts for a link the chat core cannot read" $ do
      linkHosts pageChannel `shouldBe` []
      linkHosts "https://simplex.chat/contact#/?v=2&smp=smp%3A%2F%2Fabc" `shouldBe` []
      linkHosts "hello" `shouldBe` []
  where
    refused l = (l, linkKindOf l) `shouldBe` (l, Nothing)
    padded n = let base = presetContact <> "?x=" in base <> T.replicate (n - T.length base) "a"

pageContact :: Text
pageContact = "https://smp8.simplex.im/a#Y8m3qT0vLw2Ab7cD9eF1gH3jK5mN7pQ9rS"

pageChannel :: Text
pageChannel = "simplex:/c#Rk27xN4pDe9Tz1Wb3Yc5Vd7Xf9Hg2Jk4Lm?v=1"

presetContact :: Text
presetContact = "https://smp6.simplex.im/a#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw"

multiHostContact :: Text
multiHostContact = "simplex:/a#MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY?h=smp.simplex.im%2Cjjbyvoemxysm7qxap7m5d5m35jzv5qq6gnlv7s4rsn7tdwwmuqciwpid.onion&p=5223&c=1234-w"

multiHostChannel :: Text
multiHostChannel = "simplex:/c#MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY?h=smp.simplex.im%2Cjjbyvoemxysm7qxap7m5d5m35jzv5qq6gnlv7s4rsn7tdwwmuqciwpid.onion&p=5223&c=1234-w"

onionHost :: ByteString
onionHost = "jjbyvoemxysm7qxap7m5d5m35jzv5qq6gnlv7s4rsn7tdwwmuqciwpid.onion"
