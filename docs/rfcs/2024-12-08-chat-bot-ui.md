# Chat bots UI

## Problem

Chat bots are a simple way to increase utility of the network, as it helps other people to create value by providing arbitrary services via chat interface.

Interacting with chat bots requires structured commands, which require entering them correctly, similarly how people do it in the terminal, and for most users it's hard.

## Solution

### Bot commands

As commands should be recognized based on their syntax, the same list of commands should be accepted irrespective of the conversation state. In cases commands cannot be executed, for any reason, bot would respond with the message.

The bot profile is probably the best place to include supported commands, in the form of a menu tree. See the schema.

The downside of communicating commands via profile is that they may change per contact or per group. The place to put commands would be chat preferences, which also makes sense - receiving commands is "the preference" of the bot. This preference would indicate that bot supports commands, and also which commands are supported with any additional information.

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

For protocol design we have three options:
- a completely different message content constructor,
- an extension to x.msg.new message instructing the bot to show button,
- a normal message where all information would be present in the message text so it could be correctly presented by the old clients, with additional property that would be interpreted by the new clients so that it can parse the message text to present it as a multiple choice.
- the bots can send a normal message to the old clients with a different message to the new clients, and it can be abstracted by the bot library.

The last option where the bot sends text message to the old clients and structured message to the new clients seems the most reasonable, as it avoids the need to parse texts, while preserves backward compatibility without complicating code for bot developers.

### Protocol

Add to message container an optional property `dialog`, an object with these optional properties:

- `persistent`: boolean indicating that the choice should remain after user making a choice without bot sending additional messages, and won't be removed when bot sends additional messages, unless this additional message contains a choice. This is convenient for service bots with limited number of persistent choices, e.g. directory bot that may have buttons "Menu" (or "Help") and free text entry for search queries. This menu will be removed when a message with another choice is received.
- `layout`: "row" or "column" (default). "column" layout could use standard confirmation dialogue to present choices on iOS (although it should not be modal, and should allow scrolling the conversation, but not replying or sending messages).
- `allow`: an object showing possible arbitrary message responses, currently only `text` property is allowed with `false` (default) or `true` value. Because button responses are just text messages, if free text is allowed, and user sends the same text as button, with or without surrounding spaces, the bots should interpret them in the same way.
- `choices`: an array of choices presented as buttons, with required property `text` and optional properties `tag`, `role`, `info`, `icon` and `disabled`.

See the schema in the protocol schema document.
