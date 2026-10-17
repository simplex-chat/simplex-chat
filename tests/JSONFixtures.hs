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
activeUserSwift = "{\"result\":{\"_owsf\":true,\"activeUser\":{\"user\":{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"\",\"shortDescr\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"files\":{\"allow\":\"always\"},\"calls\":{\"allow\":\"yes\"},\"sessions\":{\"allow\":\"no\"},\"commands\":[]},\"activeUser\":true,\"activeOrder\":1,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true,\"autoAcceptMemberContacts\":false,\"autoAcceptGroupInvitations\":false,\"userChatRelay\":false,\"clientService\":false}}}}"

activeUserTagged :: LB.ByteString
activeUserTagged = "{\"result\":{\"type\":\"activeUser\",\"user\":{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"\",\"shortDescr\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"files\":{\"allow\":\"always\"},\"calls\":{\"allow\":\"yes\"},\"sessions\":{\"allow\":\"no\"},\"commands\":[]},\"activeUser\":true,\"activeOrder\":1,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true,\"autoAcceptMemberContacts\":false,\"autoAcceptGroupInvitations\":false,\"userChatRelay\":false,\"clientService\":false}}}"

chatStartedSwift :: LB.ByteString
chatStartedSwift = "{\"result\":{\"_owsf\":true,\"chatStarted\":{}}}"

chatStartedTagged :: LB.ByteString
chatStartedTagged = "{\"result\":{\"type\":\"chatStarted\"}}"

connectionsDiffSwift :: LB.ByteString
connectionsDiffSwift = "{\"result\":{\"_owsf\":true,\"connectionsDiff\":{\"userIds\":{\"missingIds\":[],\"extraIds\":[]},\"connIds\":{\"missingIds\":[],\"extraIds\":[]}}}}"

connectionsDiffTagged :: LB.ByteString
connectionsDiffTagged = "{\"result\":{\"type\":\"connectionsDiff\",\"userIds\":{\"missingIds\":[],\"extraIds\":[]},\"connIds\":{\"missingIds\":[],\"extraIds\":[]}}}"

userJSON :: LB.ByteString
userJSON = "{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"\",\"shortDescr\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"files\":{\"allow\":\"always\"},\"calls\":{\"allow\":\"yes\"},\"sessions\":{\"allow\":\"no\"},\"commands\":[]},\"activeUser\":true,\"activeOrder\":1,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true,\"autoAcceptMemberContacts\":false,\"autoAcceptGroupInvitations\":false,\"userChatRelay\":false}"

parsedMarkdownSwift :: LB.ByteString
parsedMarkdownSwift = "{\"formattedText\":[{\"format\":{\"_owsf\":true,\"bold\":{}},\"text\":\"hello\"}]}"

parsedMarkdownTagged :: LB.ByteString
parsedMarkdownTagged = "{\"formattedText\":[{\"format\":{\"type\":\"bold\"},\"text\":\"hello\"}]}"

markdownMarkersSwift :: LB.ByteString
markdownMarkersSwift = "[{\"format\":{\"_owsf\":true,\"blockStart\":{\"block\":{\"_owsf\":true,\"paragraph\":{}}}},\"text\":\"\"},{\"format\":{\"_owsf\":true,\"bold\":{}},\"text\":\"p\"},{\"format\":{\"_owsf\":true,\"blockEnd\":{}},\"text\":\"\"},{\"text\":\"\\n\"},{\"format\":{\"_owsf\":true,\"blockStart\":{\"block\":{\"_owsf\":true,\"heading\":{\"level\":2}}}},\"text\":\"## \"},{\"format\":{\"_owsf\":true,\"blockStart\":{\"block\":{\"_owsf\":true,\"codeBlock\":{\"language\":\"bash\"}}}},\"text\":\"```bash\\n\"},{\"format\":{\"_owsf\":true,\"blockStart\":{\"block\":{\"_owsf\":true,\"codeBlock\":{}}}},\"text\":\"```\\n\"},{\"format\":{\"_owsf\":true,\"lineBreak\":{}},\"text\":\"\\n\"},{\"format\":{\"_owsf\":true,\"blockStart\":{\"block\":{\"_owsf\":true,\"quote\":{}}}},\"text\":\"> \"},{\"format\":{\"_owsf\":true,\"blockStart\":{\"block\":{\"_owsf\":true,\"listItem\":{\"number\":\"2.\",\"indent\":1}}}},\"text\":\"  2. \"},{\"format\":{\"_owsf\":true,\"blockStart\":{\"block\":{\"_owsf\":true,\"listItem\":{\"indent\":0}}}},\"text\":\"- \"},{\"format\":{\"_owsf\":true,\"blockStart\":{\"block\":{\"_owsf\":true,\"table\":{\"columns\":[\"none\",\"center\"]}}}},\"text\":\"| \"},{\"format\":{\"_owsf\":true,\"cellBreak\":{}},\"text\":\" | \"},{\"format\":{\"_owsf\":true,\"blockStart\":{\"block\":{\"_owsf\":true,\"rule\":{}}}},\"text\":\"---\"}]"

markdownMarkersTagged :: LB.ByteString
markdownMarkersTagged = "[{\"format\":{\"type\":\"blockStart\",\"block\":{\"type\":\"paragraph\"}},\"text\":\"\"},{\"format\":{\"type\":\"bold\"},\"text\":\"p\"},{\"format\":{\"type\":\"blockEnd\"},\"text\":\"\"},{\"text\":\"\\n\"},{\"format\":{\"type\":\"blockStart\",\"block\":{\"type\":\"heading\",\"level\":2}},\"text\":\"## \"},{\"format\":{\"type\":\"blockStart\",\"block\":{\"type\":\"codeBlock\",\"language\":\"bash\"}},\"text\":\"```bash\\n\"},{\"format\":{\"type\":\"blockStart\",\"block\":{\"type\":\"codeBlock\"}},\"text\":\"```\\n\"},{\"format\":{\"type\":\"lineBreak\"},\"text\":\"\\n\"},{\"format\":{\"type\":\"blockStart\",\"block\":{\"type\":\"quote\"}},\"text\":\"> \"},{\"format\":{\"type\":\"blockStart\",\"block\":{\"type\":\"listItem\",\"number\":\"2.\",\"indent\":1}},\"text\":\"  2. \"},{\"format\":{\"type\":\"blockStart\",\"block\":{\"type\":\"listItem\",\"indent\":0}},\"text\":\"- \"},{\"format\":{\"type\":\"blockStart\",\"block\":{\"type\":\"table\",\"columns\":[\"none\",\"center\"]}},\"text\":\"| \"},{\"format\":{\"type\":\"cellBreak\"},\"text\":\" | \"},{\"format\":{\"type\":\"blockStart\",\"block\":{\"type\":\"rule\"}},\"text\":\"---\"}]"
