# Markdown: edge cases

Date: 2026-10-29. Follows `2026-10-29-markdown-links-and-line-breaks.md`.

Haskell paths without a directory are in `src/Simplex/Chat/`.

## Decisions

```
E1   phones               a phone starts with "+" or "0"; another number of 7 to 22 characters is text
E2   domains              a link without a scheme needs a lowercase top-level domain
E3   "_" and "__"         a closing "_" followed by a letter or a digit is text (CommonMark)
E4   "#" names            digits only after "#" are text
E5   commands, iOS        plain text when the chat does not use commands, as Android and desktop
E6   quotes               ">" needs a space or a tab after it, or the line end
E7   numbered lines       after a paragraph line, only "1." or "1)" starts a list (CommonMark example 304)
E8   "---" under text     a rule; "===" under text stays a heading
E9   delimiter runs       a closing delimiter followed by the same character is text
E11  code spans           code spans inside a format are content of the format
E12  headings             the spaces after "#" are part of the heading mark
```

Unchanged: E10 nested formats, E13 links without a scheme, E14 links after quotes (GFM), E15 non-Latin links, E16 task lists.

## Inline

`formattedP c f`:

```
content    pieces to a closing c on the same line:
             text without c, "`" and line breaks
             a code span, or its backticks when it is not closed in the line
             a run of c longer than 1
             a single c followed by a letter or a digit, for "_"
otherwise  as before: across lines, or the delimiter and the rest of the line as text
```

`doubleP c f`:

```
content    text without c and "`", and code spans, to cc
closing    cc not followed by c, nor by a letter or a digit for "_"
```

Other:

```
phoneP       Phone when it starts with "+" or "0"; text when 7 to 22 characters otherwise
isDomain     the top-level domain in lowercase ASCII letters
nameRefP     "#" with digits only: not a name
```

## Blocks

```
quoteMarkP         ">", then a space or a tab, or the line end
headingMarkP       "#" run, a space, then spaces and tabs
paragraphP         continuation: a paragraph line, or a numbered list line with a number other than 1
underlineLevel     "=" only; HeadingUnderline 2 is not produced, kept for decoding
```

## iOS

`MsgContentView.swift`, `messageText`: a command is drawn as code with "/" and its name only when it can be sent; otherwise its text as typed.
