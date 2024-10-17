# Conversational agent

## Problem

SimpleX Chat has a different paradigm of connecting the users - instead of discovering friends, like it happens in most other apps, the users have to invite the friends. Also, many configuration options are complex and require explanations.

Historically, we tried different UX approaches with various amount of explanations texts, sheets that open when you press (i) icon, and external documentations.

All these approaches are ineffective, and we lose the new users during the onboarding, especially if they don't come via the introduction of the existing users - they simply fail to understand why the app design is what it is, why it cannot do what every other app is doing, and instead of using the onboarding as a promotion opportunity, we simply lose the users.

At this point, as the users are growing it appears to be the #1 blocker for the growth.

## Solution

We believe that the best approach to both onboard the users and to explain all the complex concepts while using the app is conversational assistant that offers scripted conversation with both multiple choice and free-form questions that users can interact with and both understand what options and choices best fit their needs and manage the app configuration and actions via conversational UX.

## Implementation

Possible options are:

## Just write code in the UI

Pros: it avoids any abstractions that can prove to be inadequate to the task as the requirements are discovered and evolve.

Cons: even though there are some differences between the platform, particularly around how the user connects to the network, there is a lot of overlap, and this approach would result in a lot of repetive code.

## Use JavaScript as a cross-platform language

Pros: the same code will run on all platforms.

Cons: requires complex integration and design of JS host environment to integrate both with the code code and with the UI.

While this option is very tempting, and JS engine can be used for other things - chat widgets and interactive activities - it is probably unnecessary for this problem.

## Programm it in Haskell

Pros: no new languages, easy to create high level DSL to describe conversational scenarios, and at the same time have direct access to conversation data in Haskell code, without the need for any additional API, as would be required with JavaScript.

Cons:
- UIs would have to support protocol for UI actions and settings.
- Harder to use to some developers in the team.

Overall, this appears to be the simplest and the most effective approach.

Conversation(s) with the agent would be stored in the usual database, and the agent would be interactive - it will be called by the code when the message is added to this conversation.

Questions to consider:
- ux:
  - should it be one conversation for all user profiles or separate conversations per profile? The latter seems safer from privacy and security points of view.
- types:
  - conversation type. Could use "direct" or may be a special type.
  - chat item types. Currently we do not parameterize chat items by conversation types, even though some chat items are only possible in specific conversations.
- UI protocol:
  - currently we do not have a pattern when the core executes commands in the UI. We could use the event with some correlation ID and another API call to pass the response from the UI.
  - alternatively, because all UI changes will be triggered by the user action of choosing one of the multiple choice answers we may instead attach UI commands to the buttons to avoid round trip of sending this choice to the agent and then have the agent call UI command.
