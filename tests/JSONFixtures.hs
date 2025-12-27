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
activeUserSwift = "{\"result\":{\"_owsf\":true,\"activeUser\":{\"user\":{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"\",\"shortDescr\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"files\":{\"allow\":\"always\"},\"calls\":{\"allow\":\"yes\"},\"sessions\":{\"allow\":\"no\"},\"commands\":[]},\"activeUser\":true,\"activeOrder\":1,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true,\"autoAcceptMemberContacts\":false,\"clientService\":false}}}}"

activeUserTagged :: LB.ByteString
activeUserTagged = "{\"result\":{\"type\":\"activeUser\",\"user\":{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"\",\"shortDescr\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"files\":{\"allow\":\"always\"},\"calls\":{\"allow\":\"yes\"},\"sessions\":{\"allow\":\"no\"},\"commands\":[]},\"activeUser\":true,\"activeOrder\":1,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true,\"autoAcceptMemberContacts\":false,\"clientService\":false}}}"

chatStartedSwift :: LB.ByteString
chatStartedSwift = "{\"result\":{\"_owsf\":true,\"chatStarted\":{}}}"

chatStartedTagged :: LB.ByteString
chatStartedTagged = "{\"result\":{\"type\":\"chatStarted\"}}"

connectionsDiffSwift :: LB.ByteString
connectionsDiffSwift = "{\"result\":{\"_owsf\":true,\"connectionsDiff\":{\"userIds\":{\"missingIds\":[],\"extraIds\":[]},\"connIds\":{\"missingIds\":[],\"extraIds\":[]}}}}"

connectionsDiffTagged :: LB.ByteString
connectionsDiffTagged = "{\"result\":{\"type\":\"connectionsDiff\",\"userIds\":{\"missingIds\":[],\"extraIds\":[]},\"connIds\":{\"missingIds\":[],\"extraIds\":[]}}}"

userJSON :: LB.ByteString
userJSON = "{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"\",\"shortDescr\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"files\":{\"allow\":\"always\"},\"calls\":{\"allow\":\"yes\"},\"sessions\":{\"allow\":\"no\"},\"commands\":[]},\"activeUser\":true,\"activeOrder\":1,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true,\"autoAcceptMemberContacts\":false}"

parsedMarkdownSwift :: LB.ByteString
parsedMarkdownSwift = "{\"formattedText\":[{\"format\":{\"_owsf\":true,\"bold\":{}},\"text\":\"hello\"}]}"

parsedMarkdownTagged :: LB.ByteString
parsedMarkdownTagged = "{\"formattedText\":[{\"format\":{\"type\":\"bold\"},\"text\":\"hello\"}]}"
