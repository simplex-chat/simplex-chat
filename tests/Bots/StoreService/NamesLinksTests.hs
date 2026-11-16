{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.NamesLinksTests (namesLinksTests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.List.NonEmpty as L
import Data.Text (Text)
import qualified Data.Text as T
import Simplex.Messaging.Transport.Client (TransportHost (..))
import StoreService.Names.Profiles (shortLinkHosts)
import StoreService.Names.Registration (LinkKind (..), linkMaxBytes, nameLink)
import Test.Hspec

namesLinksTests :: Spec
namesLinksTests = describe "StoreService names: links" $ do
  describe "kinds" $ do
    it "accepts the short links of each kind that the chat core reads" $ do
      kindOf presetContact `shouldBe` Just LKContact
      kindOf channel `shouldBe` Just LKChannel
      kindOf "simplex:/a#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw?h=smp6.simplex.im" `shouldBe` Just LKContact
      kindOf "https://localhost/a#4AkRDmhf64tdRlN406g8lJRg5OCmhD6ynIhi6glOcCM?p=7001&c=LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI" `shouldBe` Just LKContact
      kindOf "https://1.2.3.4/c#MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY" `shouldBe` Just LKChannel
    it "accepts what the chat core's grammar admits: an upper-case type, a slash before the key and an escaped host separator" $ do
      kindOf "https://smp6.simplex.im/A#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw" `shouldBe` Just LKContact
      kindOf "https://smp6.simplex.im/c/#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw" `shouldBe` Just LKChannel
      kindOf "simplex:/a#MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY?h=smp.simplex.im%2cjjbyvoemxysm7qxap7m5d5m35jzv5qq6gnlv7s4rsn7tdwwmuqciwpid.onion" `shouldBe` Just LKContact
    it "refuses full links, two links joined, other link types, keys and servers the chat core cannot read, and anything else" $ do
      mapM_
        refused
        [ "https://simplex.chat/contact#/?v=2&smp=smp%3A%2F%2Fabc",
          presetContact <> ";" <> presetContact,
          "hello",
          "",
          "https://smp8.simplex.im/g#MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY",
          "https://smp8.simplex.im/r#MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY",
          "https://smp8.simplex.im/i#MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3/MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY",
          "simplex:/g#MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY?h=smp.simplex.im",
          "http://smp6.simplex.im/a#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw",
          "https://smp8.simplex.im/a#Y8m3qT0vLw2Ab7cD9eF1gH3jK5mN7pQ9rS",
          "https://smp8.simplex.im/a#x",
          "https://smp8.simplex.im/a#",
          "https:///a#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw",
          noHostChannel,
          "simplex:/a#MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY?h=smp.simplex.im%2Fx",
          " " <> presetContact,
          presetContact <> "\n",
          presetContact <> " "
        ]
    it "accepts a link with several hosts, as simplexmq writes it" $ do
      kindOf multiHostContact `shouldBe` Just LKContact
      kindOf multiHostChannel `shouldBe` Just LKChannel
    it "accepts a link of exactly the bound and refuses one byte over it" $ do
      let atBound = padded linkMaxBytes
      T.length atBound `shouldBe` linkMaxBytes
      kindOf atBound `shouldBe` Just LKContact
      kindOf (padded $ linkMaxBytes + 1) `shouldBe` Nothing
  it "reads every host of a link: an https authority, an IP literal, its extra hosts, a simplex link's server hosts, a channel link's, and none of a link the chat core cannot read" $
    forM_
      [ (presetContact, [THDomainName "smp6.simplex.im"]),
        ("https://1.2.3.4/c#MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY", [THIPv4 (1, 2, 3, 4)]),
        ("https://smp.simplex.im/a#MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY?h=jjbyvoemxysm7qxap7m5d5m35jzv5qq6gnlv7s4rsn7tdwwmuqciwpid.onion&p=5223&c=1234-w", [THDomainName "smp.simplex.im", THOnionHost onionHost]),
        ("simplex:/a#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw?h=smp6.simplex.im", [THDomainName "smp6.simplex.im"]),
        (multiHostContact, [THDomainName "smp.simplex.im", THOnionHost onionHost]),
        (multiHostChannel, [THDomainName "smp.simplex.im", THOnionHost onionHost]),
        ("simplex:/c#AQIDBAUGBwgBAgMEBQYHCAECAwQFBgcIAQIDBAUGBwg?h=smp.simplex.im&p=5223&c=1234-w", [THDomainName "smp.simplex.im"]),
        (noHostChannel, []),
        ("https://simplex.chat/contact#/?v=2&smp=smp%3A%2F%2Fabc", []),
        ("hello", [])
      ]
      $ \(l, hosts) -> (l, linkHosts l) `shouldBe` (l, hosts)
  where
    refused l = (l, kindOf l) `shouldBe` (l, Nothing)
    padded n = let base = presetContact <> "?x=" in base <> T.replicate (n - T.length base) "a"

kindOf :: Text -> Maybe LinkKind
kindOf = fmap fst . nameLink

channel :: Text
channel = "https://smp9.simplex.im/c#Rk27xN4pDe9Tz1Wb3Yc5Vd7Xf9Hg2Jk4LmAAAAAAAAA"

noHostChannel :: Text
noHostChannel = "simplex:/c#Rk27xN4pDe9Tz1Wb3Yc5Vd7Xf9Hg2Jk4LmAAAAAAAAA?v=1"

presetContact :: Text
presetContact = "https://smp6.simplex.im/a#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw"

multiHostContact :: Text
multiHostContact = "simplex:/a#MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY?h=smp.simplex.im%2Cjjbyvoemxysm7qxap7m5d5m35jzv5qq6gnlv7s4rsn7tdwwmuqciwpid.onion&p=5223&c=1234-w"

multiHostChannel :: Text
multiHostChannel = "simplex:/c#MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY?h=smp.simplex.im%2Cjjbyvoemxysm7qxap7m5d5m35jzv5qq6gnlv7s4rsn7tdwwmuqciwpid.onion&p=5223&c=1234-w"

onionHost :: ByteString
onionHost = "jjbyvoemxysm7qxap7m5d5m35jzv5qq6gnlv7s4rsn7tdwwmuqciwpid.onion"

-- | The hosts as the chat core reads them, empty for a link it cannot read.
linkHosts :: Text -> [TransportHost]
linkHosts = maybe [] (L.toList . shortLinkHosts . snd) . nameLink
