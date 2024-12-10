# Chat bots UI

## Problem

Chat bots are a simple way to increase utility of the network, as it helps other people to create value by providing arbitrary services via chat interface.

Interacting with chat bots requires structured commands, which require entering them correctly, similarly how people do it in the terminal, and for most users it's hard.

## Solution

### Bot commands

As commands should be recognized based on their syntax, the same list of commands should be accepted irrespective of the conversation state. In cases commands cannot be executed, for any reason, bot would respond with the message.

One of the options where commands can be communicated is bot profile the best place to include supported commands, in the form of a menu tree. See the schema.

The downside of communicating commands via profile is that they may change per contact or per group. The usual place to put commands is chat preferences, which also make sense - receiving commands is "the preference" of the bot. This preference would indicate that bot supports commands, and also which commands are supported with any additional information.

This menu will be shown to the end users in the app when they type "/" character in the first position of after several spaces. When the user chooses command from the menu, it will be inserted into the entry field for further editing. The placeholders for parameter should be inserted as "<name>". Clients may offer some interaction for filling parameters in, but in MVP it is sufficient to let users edit them directly.

When processing commands bots should ignore leading and trailing spaces in the user messages.

### Interactive dialogues with buttons

A special message sent by chat bot that would show a modal UI requiring to choose one of several presented options. These options should contain the text visible to the user with an optional emoji or action icon. The clients may automatically use icons for known words.

When this message is received, the compose area is either replaced with or complemented (if the context allows a free text entry in response) with one or several buttons, either located in a row, with wrapping, or in a column.

When the response is sent, the chosen text is sent, possibly as a reply to the original message, and the original message can be looked at to show which choices it offered in message information pane. There could be slightly different design or some marker to indicate that it was a message that offered multiple choices, and it could be updated to indicate it was replied to.

The clients probably should not show client messages as replies. There are 3 options here:
- show as a normal reply. This avoids any ambiguity in the conversation history, and may protect customers from misbehaving bots - the message would be visually attached to the question.
- simply do not show reply in the conversation, but use replies in the protocol.
- use some other marker in the protocol.

I prefer sending and showing normal replies in these cases, as it avoids any ambiguity in the transcript about which question the choices were offered for. That means that bots should support replies, not only normal messages.

For protocol design we could either offer a completely different message, or we could use a normal message where all information would be present in the message text so it could be correctly presented by the old clients, with additional property that would be interpreted by the new clients so that it can parse the message text to present it as a multiple choice.

I prefer using normal messages with additional data.

### Protocol

Add to message container an optional property `choice`, an object with these optional properties:

- `persistent`: boolean indicating that the choice should remain after user making a choice without bot sending additional messages, and won't be removed when bot sends additional messages, unless this additional message contains a choice. This is convenient for service bots with limited number of persistent choices, e.g. directory bot that may have buttons "Menu" (or "Help") and free text entry for search queries. This menu will be removed when a message with another choice
- `layout`: "row" or "column" (default). "column" layout could use standard confirmation dialogue to present choices on iOS (although it should not be modal, and should allow scrolling the conversation, but not replying or sending messages).
- `allow`: an object showing possible arbitrary message responses, currently only `text` property is allowed with `false` (default) or `true` value. Because button responses are just text messages, if free text is allowed, and user sends the same text as button, with or without surrounding spaces, the bots should interpret them in the same way.
- `buttons`: an object mapping response texts to buttons, with optional properties `icon`, `color` and `disabled`.

See the schema in the protocol schema document.

The message content itself would include the actual list of choices (both for consistency with the message and for backward compatibility) in this format:

- message text: any number of lines.
- an empty line indicating the beggining of choices section.
- an optional non-empty line that does not start from "-" that will be interpreted as a header for choices. It could be "Choose:" or "Select:" or "Send one of:" or absolutely anything that would only be shown in the old clients as part of the message and as a title for choices in the new client (e. g., the title of confirmation dialogue).
- choices starting from "-" (hyphen) and " " (space characters). It will be shown on the button as is, unless `button` has `text` property. It will be sent in response, together with the optional tag.
```

This allows to program backwards compatible bots, e.g. for the directory service bot the message could be:

```