{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Help
  ( chatWelcome,
    chatHelpInfo,
    filesHelpInfo,
    groupsHelpInfo,
    contactsHelpInfo,
    myAddressHelpInfo,
    incognitoHelpInfo,
    messagesHelpInfo,
    remoteHelpInfo,
    markdownInfo,
    settingsInfo,
    databaseHelpInfo,
  )
where

import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Simplex.Chat.Markdown
import Simplex.Chat.Styled
import Simplex.Chat.Types (LocalProfile (..), User (..))
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
      "Connect to SimpleX Chat developers for any questions - just type " <> highlight "/simplex",
      "",
      "Follow our updates:",
      "> Reddit: https://www.reddit.com/r/SimpleXChat/",
      "> Twitter: https://twitter.com/SimpleXChat",
      "",
      "Type " <> highlight "/help" <> " for usage info, " <> highlight "/welcome" <> " to show this message"
    ]
  where
    User {profile = LocalProfile {displayName, fullName}} = user
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
      indent <> highlight "/help <topic>    " <> " - help on: " <> listHighlight ["groups", "contacts", "messages", "files", "address", "incognito", "remote", "settings", "db"],
      indent <> highlight "/profile         " <> " - show / update user profile",
      indent <> highlight "/delete <contact>" <> " - delete contact and all messages with them",
      indent <> highlight "/chats           " <> " - most recent chats",
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
      indent <> highlight "/file @<contact> <file_path>       " <> " - send file to contact",
      indent <> highlight "/file #<group> <file_path>         " <> " - send file to group",
      indent <> highlight "/image <name> [<file_path>]        " <> " - send file as image to @contact or #group",
      indent <> highlight "/freceive <file_id> [<file_path>]  " <> " - accept to receive file",
      indent <> highlight "/fforward <name> [<file_id>]       " <> " - forward received file to @contact or #group",
      indent <> highlight "/fcancel <file_id>                 " <> " - cancel sending / receiving file",
      indent <> highlight "/fstatus <file_id>                 " <> " - show file transfer status",
      indent <> highlight "/imgf @<contact> <file_id>         " <> " - forward received image to contact",
      indent <> highlight "/imgf #<group> <file_id>           " <> " - forward received image to group",
      "",
      "The commands may be abbreviated: " <> listHighlight ["/f", "/img", "/fr", "/ff", "/fc", "/fs"]
    ]

groupsHelpInfo :: [StyledString]
groupsHelpInfo =
  map
    styleMarkdown
    [ green "Group commands:",
      indent <> highlight "/group <group> [<full_name>]       " <> " - create group",
      indent <> highlight "/add <group> <contact> [<role>]    " <> " - add contact to group, roles: " <> highlight "owner" <> ", " <> highlight "admin" <> " (default), " <> highlight "member",
      indent <> highlight "/join <group>                      " <> " - accept group invitation",
      indent <> highlight "/members <group>                   " <> " - list group members",
      indent <> highlight "/remove <group> <member>           " <> " - remove member from group",
      indent <> highlight "/leave <group>                     " <> " - leave group",
      indent <> highlight "/clear #<group>                    " <> " - clear all messages in the group locally",
      indent <> highlight "/delete #<group>                   " <> " - delete group and all messages",
      indent <> highlight "/gp <group>                        " <> " - view group profile",
      indent <> highlight "/gp <group> <name> [<full_name>]   " <> " - update group profile names",
      indent <> highlight "/group_descr <group> [<descr>]     " <> " - update/remove group description",
      indent <> highlight "/groups                            " <> " - list groups",
      indent <> highlight "#<group> <message>                 " <> " - send message to group",
      "",
      green "Public group links:",
      indent <> highlight "/create link #<group> [role]       " <> " - create public group link (with optional role, default: member)",
      indent <> highlight "/set link role #<group> role       " <> " - change role assigned to the users joining via the link (member/observer)",
      indent <> highlight "/show link #<group>                " <> " - show public group link and initial member role",
      indent <> highlight "/delete link #<group>              " <> " - delete link to join the group (does NOT delete any members)",
      "",
      green "Mute group messages:",
      indent <> highlight "/mute #<group>                     " <> " - do not show contact's messages",
      indent <> highlight "/unmute #<group>                   " <> " - show contact's messages",
      "",
      green "Group member connection and security:",
      indent <> highlight "/code #<group> <member>            " <> " - show member's security code",
      indent <> highlight "/verify #<group> <member> <code>   " <> " - verify member's security code",
      indent <> highlight "/verify #<group> <member>          " <> " - clear security code verification",
      indent <> highlight "/info #<group> <member>            " <> " - info about member connection",
      indent <> highlight "/switch #<group> <member>          " <> " - switch receiving messages to another SMP relay",
      "",
      green "Group chat preferences:",
      indent <> highlight "/set voice #<group> on/off         " <> " - enable/disable voice messages",
      indent <> highlight "/set files #<group> on/off         " <> " - enable/disable files and media (other than voice)",
      indent <> highlight "/set history #<group> on/off       " <> " - enable/disable sending recent history to new members",
      indent <> highlight "/set delete #<group> on/off        " <> " - enable/disable full message deletion",
      indent <> highlight "/set direct #<group> on/off        " <> " - enable/disable direct messages to other members",
      indent <> highlight "/set disappear #<group> on <time>  " <> " - enable disappearing messages with <time>:",
      indent <> highlight "                                   " <> "   30s, 5min, 1h, 8h, day, week, month",
      indent <> highlight "/set disappear #<group> off        " <> " - disable disappearing messages",
      "",
      "The commands may be abbreviated: " <> listHighlight ["/g", "/a", "/j", "/rm", "/l", "/d", "/ms", "/gs"]
    ]

contactsHelpInfo :: [StyledString]
contactsHelpInfo =
  map
    styleMarkdown
    [ green "Contact commands:",
      indent <> highlight "/contacts                " <> " - list contacts",
      indent <> highlight "/clear @<name>           " <> " - clear all messages with the contact locally",
      indent <> highlight "/delete @<name>          " <> " - delete contact and all messages",
      "",
      green "Mute contact messages:",
      indent <> highlight "/mute @<name>            " <> " - do not show contact's messages",
      indent <> highlight "/unmute @<name>          " <> " - show contact's messages",
      "",
      green "Contact connection and security:",
      indent <> highlight "/code @<name>            " <> " - show contact's security code",
      indent <> highlight "/verify @<name> <code>   " <> " - verify contact's security code",
      indent <> highlight "/verify @<name>          " <> " - clear security code verification",
      indent <> highlight "/info @<name>            " <> " - info about contact connection",
      indent <> highlight "/switch @<name>          " <> " - switch receiving messages to another SMP relay",
      "",
      green "Contact chat preferences:",
      indent <> highlight "/set voice @<name> yes/no/always   " <> " - allow/prohibit voice messages with the contact",
      indent <> highlight "/set voice @<name>                 " <> " - reset voice messages to user's default",
      indent <> highlight "/set delete @<name> yes/no/always  " <> " - allow/prohibit full message deletion",
      indent <> highlight "/set delete @<name>                " <> " - reset full deletion to user's default",
      indent <> highlight "/set disappear @<name> yes <time>  " <> " - enable disappearing messages with <time>:",
      indent <> highlight "                                   " <> "   30s, 5min, 1h, 8h, day, week, month",
      indent <> highlight "/set disappear @<name> yes         " <> " - enable disappearing messages with offered time",
      indent <> highlight "/set disappear @<name> no          " <> " - disable disappearing messages",
      "",
      "The commands may be abbreviated: " <> listHighlight ["/d", "/i"]
    ]

myAddressHelpInfo :: [StyledString]
myAddressHelpInfo =
  map
    styleMarkdown
    [ green "Your contact address commands:",
      indent <> highlight "/address                 " <> " - create your address",
      indent <> highlight "/profile_address on/off  " <> " - share address with your contacts (it will be added to your profile)",
      indent <> highlight "/delete_address          " <> " - delete your address (accepted contacts will remain connected)",
      indent <> highlight "/show_address            " <> " - show your address",
      indent <> highlight "/accept <name>           " <> " - accept contact request",
      indent <> highlight "/reject <name>           " <> " - reject contact request",
      "",
      "Please note: you can receive spam contact requests, but it's safe to delete the address!",
      "",
      "The commands may be abbreviated: " <> listHighlight ["/ad", "/da", "/sa", "/ac", "/rc"]
    ]

incognitoHelpInfo :: [StyledString]
incognitoHelpInfo =
  map
    styleMarkdown
    [ markdown (colored Red) "/incognito" <> " command is deprecated, use commands below instead.",
      "",
      "Incognito mode protects the privacy of your main profile — you can choose to create a new random profile for each new contact.",
      "It allows having many anonymous connections without any shared data between them in a single chat profile.",
      "When you share an incognito profile with somebody, this profile will be used for the groups they invite you to.",
      "",
      green "Incognito commands:",
      indent <> highlight "/connect incognito               " <> " - create new invitation link using incognito profile",
      indent <> highlight "/connect incognito <invitation>  " <> " - accept invitation using incognito profile",
      indent <> highlight "/accept incognito <name>         " <> " - accept contact request using incognito profile",
      indent <> highlight "/simplex incognito               " <> " - connect to SimpleX Chat developers using incognito profile",
      "",
      "The commands may be abbreviated: " <> listHighlight ["/c i", "/c i <invitation>", "/ac i <name>"],
      "To find the profile used for an incognito connection, use " <> highlight "/info <contact>" <> "."
    ]

messagesHelpInfo :: [StyledString]
messagesHelpInfo =
  map
    styleMarkdown
    [ green "Show recent chats",
      indent <> highlight "/chats [N]               " <> " - the most recent N conversations (20 by default)",
      indent <> highlight "/chats all               " <> " - all conversations",
      "",
      green "Show recent messages",
      indent <> highlight "/tail @alice [N]         " <> " - the last N messages with alice (10 by default)",
      indent <> highlight "/tail #team [N]          " <> " - the last N messages in the group team",
      indent <> highlight "/tail [N]                " <> " - the last N messages in all chats",
      "",
      green "Search for messages",
      indent <> highlight "/search @alice [N] <text>" <> " - the last N messages with alice containing <text> (10 by default)",
      indent <> highlight "/search #team [N] <text> " <> " - the last N messages in the group team containing <text>",
      indent <> highlight "/search [N] <text>       " <> " - the last N messages in all chats containing <text>",
      indent <> highlight "/?" <> " can be used instead of /search",
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
      indent <> highlight "\\ @alice hi             " <> " - to delete your message to alice",
      indent <> highlight "\\ #team hi              " <> " - to delete your message in the group #team",
      "",
      green "Editing sent messages",
      "To edit your last message press up arrow, edit (keep the initial ! symbol) and press enter.",
      "To edit your most recent message that starts with \"hi\":",
      indent <> highlight "! @alice (hi) <new msg>  " <> " - to edit your message to alice",
      indent <> highlight "! #team (hi) <new msg>   " <> " - to edit your message in the group #team"
    ]

remoteHelpInfo :: [StyledString]
remoteHelpInfo =
  map
    styleMarkdown
    [ green "Remote control",
      "You can use CLI as a remote controller for a mobile app or as a remote host for a desktop app (or another CLI).",
      "For example, you can run CLI on a server and use it from a desktop computer, connecting via SSH port forwarding.",
      "",
      indent <> highlight "/set device name <name> " <> " - set CLI name for remote connections",
      "",
      green "Using as remote controller",
      indent <> highlight "/start remote host new                 " <> " - pair and connect a new remote host",
      indent <> highlight "/start remote host <id> [multicast=on] " <> " - start connection with a known (paired) remote host",
      indent <> highlight "/stop remote host new                  " <> " - cancel pairing with a new remote host",
      indent <> highlight "/stop remote host <id>                 " <> " - stop connection with connected remote host",
      indent <> highlight "/switch remote host local              " <> " - switch to using local database",
      indent <> highlight "/switch remote host <id>               " <> " - switch to connected remote host",
      indent <> highlight "/list remote hosts                     " <> " - list known remote hosts",
      indent <> highlight "/delete remote host <id>               " <> " - delete (unpair) known remote hosts - also deletes all host files from controller",
      "",
      green "Using as remote host",
      indent <> highlight "/connect remote ctrl <session_address> " <> " - connect to remote controller via the adress from /start remote host",
      indent <> highlight "/stop remote ctrl                      " <> " - stop connection with remote controller",
      indent <> highlight "/find remote ctrl                      " <> " - find known remote controller on the local network (it should be started with multicast=on)",
      indent <> highlight "/list remote ctrls                     " <> " - list known remote controllers",
      indent <> highlight "/delete remote ctrl <id>               " <> " - delete known remote controller"
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

settingsInfo :: [StyledString]
settingsInfo =
  map
    styleMarkdown
    [ green "Chat settings:",
      indent <> highlight "/network                 " <> " - show / set network access options",
      indent <> highlight "/smp                     " <> " - show / set configured SMP servers",
      indent <> highlight "/xftp                    " <> " - show / set configured XFTP servers",
      indent <> highlight "/info <contact>          " <> " - information about contact connection",
      indent <> highlight "/info #<group> <member>  " <> " - information about member connection",
      indent <> highlight "/(un)mute <contact>      " <> " - (un)mute contact, the last messages can be printed with /tail command",
      indent <> highlight "/(un)mute #<group>       " <> " - (un)mute group",
      indent <> highlight "/get stats               " <> " - get usage statistics",
      indent <> highlight "/reset stats             " <> " - reset usage statistics"
    ]

databaseHelpInfo :: [StyledString]
databaseHelpInfo =
  map
    styleMarkdown
    [ green "Database export:",
      indent <> highlight "/db export             " <> " - create database export file that can be imported in mobile apps",
      indent <> highlight "/files_folder <path>   " <> " - set files folder path to include app files in the exported archive",
      "",
      green "Database encryption:",
      indent <> highlight "/db encrypt <key>      " <> " - encrypt chat database with key/passphrase",
      indent <> highlight "/db key <current> <new>" <> " - change the key of the encrypted app database",
      indent <> highlight "/db decrypt <key>      " <> " - decrypt chat database"
    ]
