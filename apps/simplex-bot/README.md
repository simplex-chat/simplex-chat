# Simple SimpleX Chat bot example

This chat bot is a Haskell implementation of a REPL chat bot.

All you have to do to create your bot based on this example is to provide a welcome message for connecting users and a function of type `Contact -> String -> IO String`. This function should transform the sent message into a reply message, ignoring any system messages related to preferences and user profile changes.

This bot example calculates the square of the number that is sent to it, but you can program it to do other things, simply by changing REPL function:

- a more advanced calculator (e.g., based on [this one](https://github.com/jonathanknowles/haskell-calculator)).
- translation to/from any language.
- lookup of market quotes.
- search of the information.
- AI-powered dialogue – the bot can maintain any conversation state based on the contact.
- provide any other online service via chat UI.
- etc.

Please share any bots you create with us, we will add to this page and can host them if you like!
