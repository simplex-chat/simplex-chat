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
    [ highlight "Using Simplex chat prototype.",
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
      "Simplex chat supports " <> green "file transfer" <> " and " <> green "groups" <> " functionality:",
      indent <> highlight "/help_files      " <> " - show file transfer help.",
      indent <> highlight "/help_groups     " <> " - show groups help.",
      "",
      green "Other commands:",
      indent <> highlight "/delete <contact>" <> " - delete contact and all messages with them.",
      indent <> highlight "/profile         " <> " - show / update user profile.",
      indent <> highlight "/markdown        " <> " - show supported markdown syntax.",
      indent <> highlight "/quit            " <> " - quit chat.",
      "",
      "The commands may be abbreviated to a single letter (first letters): " <> listHighlight ["/c", "/d", "/p", "/h", "/hf", "/hg"] <> ", etc."
    ]

filesHelpInfo :: [StyledString]
filesHelpInfo =
  map
    styleMarkdown
    [ green "File transfer commands:",
      indent <> highlight "/file @<contact> <file_path>         " <> " - send file to contact.",
      indent <> highlight "/file #<group> <file_path>           " <> " - send file to group.",
      indent <> highlight "/file_receive <file_id> [<file_path>]" <> " - accept to receive file.",
      indent <> highlight "/file_cancel <file_id>               " <> " - cancel sending file.",
      indent <> highlight "/file_status <file_id>               " <> " - show file transfer status."
    ]

groupsHelpInfo :: [StyledString]
groupsHelpInfo =
  map
    styleMarkdown
    [ green "Group management commands:",
      indent <> highlight "/group <group> [<full_name>]   " <> " - create group.",
      indent <> highlight "/add <group> <contact> [<role>]" <> " - add contact to group as a member, roles: " <> listHighlight ["owner", "admin", "normal"] <> ".",
      indent <> highlight "/join <group>                  " <> " - accept group invitation.",
      indent <> highlight "/remove <group> <member>       " <> " - remove member from group.",
      indent <> highlight "/leave <group>                 " <> " - leave group.",
      indent <> highlight "/delete <group>                " <> " - delete group.",
      indent <> highlight "/members <group>               " <> " - list group members.",
      indent <> highlight "#<group> <message>             " <> " - send message to group."
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
