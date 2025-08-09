# Chat widgets and activities

## Problems

A short-term problem is to decide and implement support for bots in the apps UI. Just released v6.4.3 includes support for bot commands, but the most commonly used UI approach for Telegram bots is inline buttons.

They are, effectively, simple widgets constructed as multiple lines of buttons. While currently Telegram offers web apps for bots, they are much more complex to develop for both ourselves and for bot owners, and they have many issues with security model.

Inline buttons "pros":
- super simple to develop for bot owners - they are just a 2D array of buttons with configuration, where a button can send a visible or invisible message to a bot, which in turn can update buttons however it wants.
- allow for quite advanced interfaces, with multiple layers of navigation.
- relatively simple to implement in the app.

Inline buttons "cons":
- very bad visual design.
- very limiting, compared with full inline widgets.

A longer-term problem is more advanced user activities with the bot and between each other, that could include:
- polls,
- "doodles" (see doodle.com),
- more advanced bot UIs,
- mini-games,
- etc.

## Solution

A general UX pattern that may solve both problems is "inline chat widgets".

For the examples of the possible use cases for inline chat widgets see https://webxdc.org/apps/

Problems with web apps/webxdc.
- JavaScript/Web have large binary size, that is hard to justify unless an app is a browser as well (which is not impossible),
- JavaScript has a complex security model,
- Widget size are likely to be larger than our usual message size (~15kb after compression), so they have to be sent either as files or as multiple messages.

Irrespective of what technology is used to implement widgets, there are likely to be two kinds of widgets:
- bot UI. In this case a widget can be fully controlled and even replaced by the bot (the sender), and can also process user and message events. These widgets do not necessarily need to be "an activity" (see the next), as there is no much scrolling in the chat with the bot, but they may also benefit from being marked as "active" in the same way.
- Widgets sent by users. In this case widget once sent cannot be replaced, but it can react to events, both from the sender and from the recipients. In this case, widgets have to be linked with "chat activities", so they can be easily discovered and accessed from any place in the chat while they are active, without scrolling to the point where they were started, and with an additional message posted to the chat once they complete.

This RFC describes both the widget security and execution model, and also a possible implementation approach.

## Widget security and execution models

Widgets are code that is sent by untrusted parties (or parties with the limited trust) to the users devices, so it should not be treated as trusted code.

Rather than defining what widgets code should be prevented from doing, we should define what it can do, its execution model and lifecycle.

At any point in time widget has:
- code - this is fixed, and cannot be changed.
- state - this is variable, and can be changed as described below.

Widget can react to user and message, as shown on the diagram.

![Widget events](./diagrams/2025-08-09-widget-state-machine.svg)

WL stands for "Widget Library", more on that below.

There are should be the following restrictions to widget events/processing:
- only user events (actions) can trigger sending messages, to prevent different instances of the same widget in the chat endlessly "talking" to each other.
- only one message from each remote chat peer can be sent "to the widget" to update its state. "To the widget" means that the message with event would reference the message with the widget by shared message ID that all peers in the chat have. If the widget already processed a state update it would update further state updates from this peer until user action is processed.
- once user performs some actions on the widget, further events can be processed from the same participants who previously sent events.

This execution model prevents abuse when widget state update can be requested multiple times. At the same time, this execution model allows for all necessary interactions, including UI updates by bot in response to user actions, polls (each user would only be able to send one poll event), and two- or multi-party games where "moves" have to be made in turns - each client would know that it should not send any events until it receives moves from all parties, and other parties won't be sending events too.

That all raises several questions:
- which layer should be enforcing this execution model. It can't be widget "code", as this is what we are defending from. It can be either core, or widget library, or better both.
- for direct chats, both peers can participate and send message events "to widget". While each party has its own instance of widget, with its own state, they would arrive to consistent state (not necessarily the same state, as it can be programmed to be different), once they process events. But for groups, there probably needs to be two options - 1. any party can participate (e.g., doodle or poll). 2. only pre-defined peers (by group member ID) can participate. This model works for business chat where bot, customer and multiple business agents can participate, but only customer and the bot would interact with the UI widget. 3. up to a certain number of peers can participate, but it's not defined in advance who they are. This model can work for multi-party games that can be entered by a certain number of members, but it's not defined in advance which ones. Once they enter, they would be fixed and will have to send events in turn.
- another question about execution model is access to any client data. On one hand, it may be used to improve user experience. On another hand, we have to ensure that data that is received from client device can participate in view computation, but cannot participate in computation of sent message. There have been ideas of data tainting (when each piece from client device is "tainted", and result of any computation where tainted data participates becomes tainted too, and tainted messages cannot be sent), but if we go this it has to be enforced outside of bot code, so that bot code cannot remove "tainting". Such client data could be dark or light color scheme, app and system language to localize UI, timezone, screen size. Asking user permission to access this data is a bad idea, as even if it is granted it can still be used to fingerprint users.

## Proposed implementation model

### Widget programming language

Using JavaScript or any other traditional language is problematic, as they are all general purpose languages that cannot be sufficiently constrained to ensure that they comply with the execution model. Data tainting idea would be particularly hard to implement. One language that can achieve what is required is the language where code is data that can be analysed, sanitized and constrained in its execution, and where tainted data cannot be untainted by untrusted code - Lisp. One variant of Lisp particularly stands out - PicoLisp, due to its simplicity, maturity and the existence of very advanced libraries.

While in its current state [PicoLisp](https://picolisp.com/) can only be run as a standalone process, it is feasible to execute it as a library. Even though it is not multithreaded, it is not required as widget execution can be queued, and Lisp execution environment is stateless - it receives widget state, participation state (who can participate and who already sent events - not in diagram) and events, computes new state, view and an optional message to send, and stops. In addition to that PicoLisp can be "hardened" to prevent it from crashing in Widget code, from accessing files and sensors, etc..

To achieve the required security, it is might be that Widget code needs to be interpreted by Lisp library that in its turn needs to be interpreted by PicoLisp, but possibly there can be more efficient approaches for secure code execution - e.g., it could analyse some part of the expressions and decide if to continue execution on the boundaries of some functions (e.g., those that can be called recursively, to protect against endless recursion of widget code).

The design of this "framework"/widget library is not sufficiently clear, and some bottom-up exploration is needed.

### Widget UI rendering

PicoLisp has libraries to render on canvas, HTML and SVG. We still likely need a UI library to render UI primitives without them being part of widget code. One possible option is [Nuklear](https://immediate-mode-ui.github.io/Nuklear/) - a self-contained C library that renders UI elements with event handlers in a platform-independent way, without any specific platform adapters. It can either return its own commands or OpenGL instructions that can then be converted to bitmaps.

We need to explore other options. Also see [this comment](https://github.com/Immediate-Mode-UI/Nuklear/issues/824#issuecomment-3151907607).
