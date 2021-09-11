{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Help
  ( chatHelpInfo,
    filesHelpInfo,
    groupsHelpInfo,
    markdownInfo,
  )
where

import Data.List (intersperse)
import Data.Text (Text)
import Simplex.Chat.Markdown
import Simplex.Chat.Styled
import System.Console.ANSI.Types

highlight :: Text -> Markdown
highlight = Markdown (Colored Cyan)

green :: Text -> Markdown
green = Markdown (Colored Green)

indent :: Markdown
indent = "        "

listHighlight :: [Text] -> Markdown
listHighlight = mconcat . intersperse ", " . map highlight

chatHelpInfo :: [StyledString]
chatHelpInfo =
  map
    styleMarkdown
    [ highlight "Using SimpleX chat prototype",
      "Follow these steps to set up a connection:",
      "",
      green "Step 1: " <> highlight "/connect" <> " - Alice adds a contact.",
      indent <> "Alice should send the invitation printed by the /add command",
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
      green "To send file:",
      indent <> highlight "/file bob ./photo.jpg" <> " - Alice sends file to Bob",
      indent <> "File commands: " <> highlight "/help files",
      "",
      green "To create group:",
      indent <> highlight "/group team" <> " - create group #team",
      indent <> "Group commands: " <> highlight "/help groups",
      "",
      green "Other commands:",
      indent <> highlight "/profile         " <> " - show user profile",
      indent <> highlight "/profile <name> [<full_name>]" <> " - update user profile",
      indent <> highlight "/delete <contact>" <> " - delete contact and all messages with them",
      indent <> highlight "/markdown        " <> " - show supported markdown syntax",
      indent <> highlight "/quit            " <> " - quit chat",
      "",
      "The commands may be abbreviated to a single letter: " <> listHighlight ["/c", "/f", "/g", "/p", "/h"] <> ", etc."
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
      indent <> highlight "#<group> <message>             " <> " - send message to group",
      "",
      "The commands may be abbreviated: " <> listHighlight ["/g", "/a", "/j", "/rm", "/l", "/d", "/ms"]
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
