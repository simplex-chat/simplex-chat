{-# LANGUAGE OverloadedStrings #-}

module JSONFixtures where

import qualified Data.ByteString.Lazy.Char8 as LB

noActiveUserSwift :: LB.ByteString
noActiveUserSwift = "{\"error\":{\"_owsf\":true,\"error\":{\"errorType\":{\"_owsf\":true,\"noActiveUser\":{}}}}}"

noActiveUserTagged :: LB.ByteString
noActiveUserTagged = "{\"error\":{\"type\":\"error\",\"errorType\":{\"type\":\"noActiveUser\"}}}"

activeUserExistsSwift :: LB.ByteString
activeUserExistsSwift = "{\"error\":{\"_owsf\":true,\"error\":{\"errorType\":{\"_owsf\":true,\"userExists\":{\"contactName\":\"alice\"}}}}}"

activeUserExistsTagged :: LB.ByteString
activeUserExistsTagged = "{\"error\":{\"type\":\"error\",\"errorType\":{\"type\":\"userExists\",\"contactName\":\"alice\"}}}"

activeUserSwift :: LB.ByteString
activeUserSwift = "{\"result\":{\"_owsf\":true,\"activeUser\":{\"user\":{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"calls\":{\"allow\":\"yes\"}},\"activeUser\":true,\"activeOrder\":1,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true}}}}"

activeUserTagged :: LB.ByteString
activeUserTagged = "{\"result\":{\"type\":\"activeUser\",\"user\":{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"calls\":{\"allow\":\"yes\"}},\"activeUser\":true,\"activeOrder\":1,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true}}}"

chatStartedSwift :: LB.ByteString
chatStartedSwift = "{\"result\":{\"_owsf\":true,\"chatStarted\":{}}}"

chatStartedTagged :: LB.ByteString
chatStartedTagged = "{\"result\":{\"type\":\"chatStarted\"}}"

networkStatusesSwift :: LB.ByteString
networkStatusesSwift = "{\"result\":{\"_owsf\":true,\"networkStatuses\":{\"user_\":" <> userJSON <> ",\"networkStatuses\":[]}}}"

networkStatusesTagged :: LB.ByteString
networkStatusesTagged = "{\"result\":{\"type\":\"networkStatuses\",\"user_\":" <> userJSON <> ",\"networkStatuses\":[]}}"

userJSON :: LB.ByteString
userJSON = "{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"calls\":{\"allow\":\"yes\"}},\"activeUser\":true,\"activeOrder\":1,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true}"

memberSubSummarySwift :: LB.ByteString
memberSubSummarySwift = "{\"result\":{\"_owsf\":true,\"memberSubSummary\":{\"user\":" <> userJSON <> ",\"memberSubscriptions\":[]}}}"

memberSubSummaryTagged :: LB.ByteString
memberSubSummaryTagged = "{\"result\":{\"type\":\"memberSubSummary\",\"user\":" <> userJSON <> ",\"memberSubscriptions\":[]}}"

userContactSubSummarySwift :: LB.ByteString
userContactSubSummarySwift = "{\"result\":{\"_owsf\":true,\"userContactSubSummary\":{\"user\":" <> userJSON <> ",\"userContactSubscriptions\":[]}}}"

userContactSubSummaryTagged :: LB.ByteString
userContactSubSummaryTagged = "{\"result\":{\"type\":\"userContactSubSummary\",\"user\":" <> userJSON <> ",\"userContactSubscriptions\":[]}}"

pendingSubSummarySwift :: LB.ByteString
pendingSubSummarySwift = "{\"result\":{\"_owsf\":true,\"pendingSubSummary\":{\"user\":" <> userJSON <> ",\"pendingSubscriptions\":[]}}}"

pendingSubSummaryTagged :: LB.ByteString
pendingSubSummaryTagged = "{\"result\":{\"type\":\"pendingSubSummary\",\"user\":" <> userJSON <> ",\"pendingSubscriptions\":[]}}"

parsedMarkdownSwift :: LB.ByteString
parsedMarkdownSwift = "{\"formattedText\":[{\"format\":{\"_owsf\":true,\"bold\":{}},\"text\":\"hello\"}]}"

parsedMarkdownTagged :: LB.ByteString
parsedMarkdownTagged = "{\"formattedText\":[{\"format\":{\"type\":\"bold\"},\"text\":\"hello\"}]}"
