{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Help
  ( chatWelcome,
    adminWelcomeMessages,
    chatHelpInfo,
    filesHelpInfo,
    groupsHelpInfo,
    myAddressHelpInfo,
    markdownInfo,
  )
where

import Data.ByteString (ByteString)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Simplex.Chat.Markdown
import Simplex.Chat.Styled
import Simplex.Chat.Types (Onboarding (..), Profile (..), User (..))
import System.Console.ANSI.Types

highlight :: Text -> Markdown
highlight = Markdown (Colored Cyan)

green :: Text -> Markdown
green = Markdown (Colored Green)

indent :: Markdown
indent = "        "

listHighlight :: [Text] -> Markdown
listHighlight = mconcat . intersperse ", " . map highlight

chatWelcome :: User -> Onboarding -> [StyledString]
chatWelcome user Onboarding {contactsCount, createdGroups, membersCount, filesSentCount, addressCount} =
  map
    styleMarkdown
    [ "                             __   __",
      "  ___ ___ __  __ ___ _    ___" <> "\\ \\ / /" <> " ___ _  _   _ _____",
      " / __|_ _|  \\/  | _ \\ |  | __ " <> "\\ V /" <> " / __| || | /_\\_   _|",
      " \\__ \\| || |\\/| |  _/ |__| _|" <> " / . \\" <> "| (__| __ |/ _ \\| |",
      " |___/___|_|  |_|_| |____|___" <> "/_/ \\_\\" <> "\\___|_||_/_/ \\_\\_|",
      "",
      "Welcome " <> green userName <> "!",
      "Thank you for installing SimpleX Chat!",
      "",
      "We have created several groups that you can join to play with SimpleX Chat:",
      highlight "#simplex" <> " (SimpleX Engineers 💻) - technical questions about running or contributing to SimpleX Chat",
      highlight "#hacks" <> " (Ethical Hacking 🔓) - chatting about privacy, security, announced vulnerabilities etc.",
      highlight "#music" <> " (Music 🎸) - favorite music of our team and users",
      highlight "#rand" <> " (Random 😇) - anything interesting, just keep it decent and friendly please :)",
      "",
      "Connect to our groups admin to be added to these groups - " <> highlight "/admin",
      "",
      "To continue:",
      "[" <> check (contactsCount >= 2) <> "] " <> highlight "/connect" <> " with 2 friends - " <> highlight "/help" <> " for instructions",
      "[" <> check (createdGroups >= 1 && membersCount >= 2) <> "] create a " <> highlight "/group" <> " with them - " <> highlight "/g #friends",
      "[" <> check (filesSentCount >= 1) <> "] send " <> highlight "/file" <> ", e.g. your photo, to the group - " <> highlight "/f #friends ./photo.jpg",
      "[" <> check (addressCount >= 1) <> "] create your optional chat " <> highlight "/address" <> " and share it with your friends - " <> highlight "/ad",
      "",
      "To help us build SimpleX Chat:",
      "> star GitHub repo: https://github.com/simplex-chat/simplex-chat",
      "> join Reddit group: https://www.reddit.com/r/SimpleXChat/",
      "",
      "To show this message again - " <> highlight "/welcome" <> " (or " <> highlight "/w" <> ")"
    ]
  where
    User {profile = Profile {displayName, fullName}} = user
    userName = if T.null fullName then displayName else fullName
    check c = if c then green "*" else " "

adminWelcomeMessages :: [ByteString]
adminWelcomeMessages =
  [ "Hello - and welcome to SimpleX Chat!",
    "Which community groups you'd like to join:",
    "!5 #simplex!, !5 #hacks!, !5 #music! or !5 #rand!"
  ]

chatHelpInfo :: [StyledString]
chatHelpInfo =
  map
    styleMarkdown
    [ highlight "Using SimpleX Chat",
      "Follow these steps to set up a connection:",
      "",
      green "Step 1: " <> highlight "/connect" <> " - Alice adds a contact.",
      indent <> "Alice should send the one-time invitation printed by the /connect command",
      indent <> "to her contact, Bob, out-of-band, via any trusted channel.",
      "",
      green "Step 2: " <> highlight "/connect <invitation>" <> " - Bob accepts the invitation.",
      indent <> "Bob should use the invitation he received out-of-band.",
      "",
      green "Step 3: " <> "Bob and Alice are notified that the connection is set up,",
      indent <> "both can now send messages:",
      indent <> highlight "@bob Hello, Bob!" <> " - Alice messages Bob (assuming Bob has display name 'bob').",
      indent <> highlight "@alice Hey, Alice!" <> " - Bob replies to Alice.",
      "",
      green "Send file: " <> highlight "/file bob ./photo.jpg",
      "",
      green "Create group: " <> highlight "/group team",
      "",
      green "Create your address: " <> highlight "/address",
      "",
      green "Other commands:",
      indent <> highlight "/help <topic>    " <> " - help on: files, groups, address",
      indent <> highlight "/profile         " <> " - show / update user profile",
      indent <> highlight "/delete <contact>" <> " - delete contact and all messages with them",
      indent <> highlight "/contacts        " <> " - list contacts",
      indent <> highlight "/markdown        " <> " - supported markdown syntax",
      indent <> highlight "/version         " <> " - SimpleX Chat version",
      indent <> highlight "/quit            " <> " - quit chat",
      "",
      "The commands may be abbreviated: " <> listHighlight ["/c", "/f", "/g", "/p", "/ad"] <> ", etc."
    ]

filesHelpInfo :: [StyledString]
filesHelpInfo =
  map
    styleMarkdown
    [ green "File transfer commands:",
      indent <> highlight "/file @<contact> <file_path>     " <> " - send file to contact",
      indent <> highlight "/file #<group> <file_path>       " <> " - send file to group",
      indent <> highlight "/freceive <file_id> [<file_path>]" <> " - accept to receive file",
      indent <> highlight "/fcancel <file_id>               " <> " - cancel sending / receiving file",
      indent <> highlight "/fstatus <file_id>               " <> " - show file transfer status",
      "",
      "The commands may be abbreviated: " <> listHighlight ["/f", "/fr", "/fc", "/fs"]
    ]

groupsHelpInfo :: [StyledString]
groupsHelpInfo =
  map
    styleMarkdown
    [ green "Group management commands:",
      indent <> highlight "/group <group> [<full_name>]   " <> " - create group",
      indent <> highlight "/add <group> <contact> [<role>]" <> " - add contact to group, roles: " <> highlight "owner" <> ", " <> highlight "admin" <> " (default), " <> highlight "member",
      indent <> highlight "/join <group>                  " <> " - accept group invitation",
      indent <> highlight "/remove <group> <member>       " <> " - remove member from group",
      indent <> highlight "/leave <group>                 " <> " - leave group",
      indent <> highlight "/delete <group>                " <> " - delete group",
      indent <> highlight "/members <group>               " <> " - list group members",
      indent <> highlight "/groups                        " <> " - list groups",
      indent <> highlight "#<group> <message>             " <> " - send message to group",
      "",
      "The commands may be abbreviated: " <> listHighlight ["/g", "/a", "/j", "/rm", "/l", "/d", "/ms", "/gs"]
    ]

myAddressHelpInfo :: [StyledString]
myAddressHelpInfo =
  map
    styleMarkdown
    [ green "Your contact address commands:",
      indent <> highlight "/address       " <> " - create your address",
      indent <> highlight "/delete_address" <> " - delete your address (accepted contacts will remain connected)",
      indent <> highlight "/show_address  " <> " - show your address",
      indent <> highlight "/accept <name> " <> " - accept contact request",
      indent <> highlight "/reject <name> " <> " - reject contact request",
      "",
      "Please note: you can receive spam contact requests, but it's safe to delete the address!",
      "",
      "The commands may be abbreviated: " <> listHighlight ["/ad", "/da", "/sa", "/ac", "/rc"]
    ]

markdownInfo :: [StyledString]
markdownInfo =
  map
    styleMarkdown
    [ green "Markdown:",
      indent <> highlight "*bold*         " <> " - " <> Markdown Bold "bold text",
      indent <> highlight "_italic_       " <> " - " <> Markdown Italic "italic text" <> " (shown as underlined)",
      indent <> highlight "+underlined+   " <> " - " <> Markdown Underline "underlined text",
      indent <> highlight "~strikethrough~" <> " - " <> Markdown StrikeThrough "strikethrough text" <> " (shown as inverse)",
      indent <> highlight "`code snippet` " <> " - " <> Markdown Snippet "a + b // no *markdown* here",
      indent <> highlight "!1 text!       " <> " - " <> Markdown (Colored Red) "red text" <> " (1-6: red, green, blue, yellow, cyan, magenta)",
      indent <> highlight "#secret#       " <> " - " <> Markdown Secret "secret text" <> " (can be copy-pasted)"
    ]
