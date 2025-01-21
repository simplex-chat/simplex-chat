{-# LANGUAGE OverloadedStrings #-}

module JSONFixtures where

import qualified Data.ByteString.Lazy.Char8 as LB

noActiveUserSwift :: LB.ByteString
noActiveUserSwift = "{\"resp\":{\"_owsf\":true,\"chatCmdError\":{\"chatError\":{\"_owsf\":true,\"error\":{\"errorType\":{\"_owsf\":true,\"noActiveUser\":{}}}}}}}"

noActiveUserTagged :: LB.ByteString
noActiveUserTagged = "{\"resp\":{\"type\":\"chatCmdError\",\"chatError\":{\"type\":\"error\",\"errorType\":{\"type\":\"noActiveUser\"}}}}"

activeUserExistsSwift :: LB.ByteString
activeUserExistsSwift = "{\"resp\":{\"_owsf\":true,\"chatCmdError\":{\"user_\":{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"calls\":{\"allow\":\"yes\"}},\"activeUser\":true,\"activeOrder\":1,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true},\"chatError\":{\"_owsf\":true,\"error\":{\"errorType\":{\"_owsf\":true,\"userExists\":{\"contactName\":\"alice\"}}}}}}}"

activeUserExistsTagged :: LB.ByteString
activeUserExistsTagged = "{\"resp\":{\"type\":\"chatCmdError\",\"user_\":{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"calls\":{\"allow\":\"yes\"}},\"activeUser\":true,\"activeOrder\":1,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true},\"chatError\":{\"type\":\"error\",\"errorType\":{\"type\":\"userExists\",\"contactName\":\"alice\"}}}}"

activeUserSwift :: LB.ByteString
activeUserSwift = "{\"resp\":{\"_owsf\":true,\"activeUser\":{\"user\":{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"calls\":{\"allow\":\"yes\"}},\"activeUser\":true,\"activeOrder\":1,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true}}}}"

activeUserTagged :: LB.ByteString
activeUserTagged = "{\"resp\":{\"type\":\"activeUser\",\"user\":{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"calls\":{\"allow\":\"yes\"}},\"activeUser\":true,\"activeOrder\":1,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true}}}"

chatStartedSwift :: LB.ByteString
chatStartedSwift = "{\"resp\":{\"_owsf\":true,\"chatStarted\":{}}}"

chatStartedTagged :: LB.ByteString
chatStartedTagged = "{\"resp\":{\"type\":\"chatStarted\"}}"

networkStatusesSwift :: LB.ByteString
networkStatusesSwift = "{\"resp\":{\"_owsf\":true,\"networkStatuses\":{\"user_\":" <> userJSON <> ",\"networkStatuses\":[]}}}"

networkStatusesTagged :: LB.ByteString
networkStatusesTagged = "{\"resp\":{\"type\":\"networkStatuses\",\"user_\":" <> userJSON <> ",\"networkStatuses\":[]}}"

userJSON :: LB.ByteString
userJSON = "{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"calls\":{\"allow\":\"yes\"}},\"activeUser\":true,\"activeOrder\":1,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true}"

memberSubSummarySwift :: LB.ByteString
memberSubSummarySwift = "{\"resp\":{\"_owsf\":true,\"memberSubSummary\":{\"user\":" <> userJSON <> ",\"memberSubscriptions\":[]}}}"

memberSubSummaryTagged :: LB.ByteString
memberSubSummaryTagged = "{\"resp\":{\"type\":\"memberSubSummary\",\"user\":" <> userJSON <> ",\"memberSubscriptions\":[]}}"

userContactSubSummarySwift :: LB.ByteString
userContactSubSummarySwift = "{\"resp\":{\"_owsf\":true,\"userContactSubSummary\":{\"user\":" <> userJSON <> ",\"userContactSubscriptions\":[]}}}"

userContactSubSummaryTagged :: LB.ByteString
userContactSubSummaryTagged = "{\"resp\":{\"type\":\"userContactSubSummary\",\"user\":" <> userJSON <> ",\"userContactSubscriptions\":[]}}"

pendingSubSummarySwift :: LB.ByteString
pendingSubSummarySwift = "{\"resp\":{\"_owsf\":true,\"pendingSubSummary\":{\"user\":" <> userJSON <> ",\"pendingSubscriptions\":[]}}}"

pendingSubSummaryTagged :: LB.ByteString
pendingSubSummaryTagged = "{\"resp\":{\"type\":\"pendingSubSummary\",\"user\":" <> userJSON <> ",\"pendingSubscriptions\":[]}}"

parsedMarkdownSwift :: LB.ByteString
parsedMarkdownSwift = "{\"formattedText\":[{\"format\":{\"_owsf\":true,\"bold\":{}},\"text\":\"hello\"}]}"

parsedMarkdownTagged :: LB.ByteString
parsedMarkdownTagged = "{\"formattedText\":[{\"format\":{\"type\":\"bold\"},\"text\":\"hello\"}]}"
