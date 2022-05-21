{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Help
  ( chatWelcome,
    chatHelpInfo,
    filesHelpInfo,
    groupsHelpInfo,
    myAddressHelpInfo,
    messagesHelpInfo,
    markdownInfo,
  )
where

import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Simplex.Chat.Markdown
import Simplex.Chat.Styled
import Simplex.Chat.Types (Profile (..), User (..))
import System.Console.ANSI.Types

highlight :: Text -> Markdown
highlight = markdown (colored Cyan)

green :: Text -> Markdown
green = markdown (colored Green)

indent :: Markdown
indent = "        "

listHighlight :: [Text] -> Markdown
listHighlight = mconcat . intersperse ", " . map highlight

chatWelcome :: User -> [StyledString]
chatWelcome user =
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
      "Connect to SimpleX Chat lead developer for any questions - just type " <> highlight "/simplex",
      "",
      "Follow our updates:",
      "> Reddit: https://www.reddit.com/r/SimpleXChat/",
      "> Twitter: https://twitter.com/SimpleXChat",
      "",
      "Type " <> highlight "/help" <> " for usage info, " <> highlight "/welcome" <> " to show this message"
    ]
  where
    User {profile = Profile {displayName, fullName}} = user
    userName = if T.null fullName then displayName else fullName

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
      indent <> highlight "/help <topic>    " <> " - help on: " <> listHighlight ["messages", "files", "groups", "address"],
      indent <> highlight "/profile         " <> " - show / update user profile",
      indent <> highlight "/delete <contact>" <> " - delete contact and all messages with them",
      indent <> highlight "/contacts        " <> " - list contacts",
      indent <> highlight "/smp_servers     " <> " - show / set custom SMP servers",
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
      indent <> highlight "/image <name> [<file_path>]      " <> " - send file as image to @contact or #group",
      indent <> highlight "/freceive <file_id> [<file_path>]" <> " - accept to receive file",
      indent <> highlight "/fforward <name> [<file_id>]   " <> " - forward received file to contact",
      indent <> highlight "/fcancel <file_id>               " <> " - cancel sending / receiving file",
      indent <> highlight "/fstatus <file_id>               " <> " - show file transfer status",
      "",
      "The commands may be abbreviated: " <> listHighlight ["/f", "img", "/fr", "/ff", "/fc", "/fs"]
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

messagesHelpInfo :: [StyledString]
messagesHelpInfo =
  map
    styleMarkdown
    [ green "Show recent messages",
      indent <> highlight "/tail @alice [N]" <> " - the last N messages with alice (10 by default)",
      indent <> highlight "/tail #team [N] " <> " - the last N messages in the group team",
      indent <> highlight "/tail [N]       " <> " - the last N messages in all chats",
      "",
      green "Sending replies to messages",
      "To quote a message that starts with \"hi\":",
      indent <> highlight "> @alice (hi) <msg>      " <> " - to reply to alice's most recent message",
      indent <> highlight ">> @alice (hi) <msg>     " <> " - to quote user's most recent message to alice",
      indent <> highlight "> #team (hi) <msg>       " <> " - to quote most recent message in the group from any member",
      indent <> highlight "> #team @alice (hi) <msg>" <> " - to quote alice's most recent message in the group #team",
      "",
      green "Deleting sent messages (for everyone)",
      "To delete a message that starts with \"hi\":",
      indent <> highlight "\\ @alice hi              " <> " - to delete your message to alice",
      indent <> highlight "\\ #team hi               " <> " - to delete your message in the group #team",
      "",
      green "Editing sent messages",
      "To edit your last message press up arrow, edit (keep the initial ! symbol) and press enter.",
      "To edit your most recent message that starts with \"hi\":",
      indent <> highlight "! @alice (hi) <new msg>  " <> " - to edit your message to alice",
      indent <> highlight "! #team (hi) <new msg>   " <> " - to edit your message in the group #team"
    ]

markdownInfo :: [StyledString]
markdownInfo =
  map
    styleMarkdown
    [ green "Markdown:",
      indent <> highlight "*bold*         " <> " - " <> markdown Bold "bold text",
      indent <> highlight "_italic_       " <> " - " <> markdown Italic "italic text" <> " (shown as underlined)",
      indent <> highlight "~strikethrough~" <> " - " <> markdown StrikeThrough "strikethrough text" <> " (shown as inverse)",
      indent <> highlight "`code snippet` " <> " - " <> markdown Snippet "a + b // no *markdown* here",
      indent <> highlight "!1 text!       " <> " - " <> markdown (colored Red) "red text" <> " (1-6: red, green, blue, yellow, cyan, magenta)",
      indent <> highlight "#secret#       " <> " - " <> markdown Secret "secret text" <> " (can be copy-pasted)"
    ]
