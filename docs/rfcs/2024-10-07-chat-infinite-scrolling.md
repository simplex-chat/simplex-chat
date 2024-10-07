# Chat infinite scrolling

## Problem

The current scroll-based data loading model, where new messages are loaded as users scroll upwards, introduces several usability challenges for the app:
- The system cannot reliably open a chat at the first unread message on a given chat because data may not yet be loaded for that page.
- Features like tapping on a quote to jump to older messages are not supported due to the absence of a stable mechanism to navigate across distant points in the chat history.
- When users search for a message, the results are shown without surrounding context, severely limiting the usefullness of this feature

## Solution

Adopting a bi-directional infinite scroll system that fetches and displays chat data on demand as users scroll both upwards and downwards.

This approach will directly impact all users by enabling:
- **Seamless navigation:** Users can jump to any point in the chat history (e.g., first unread message or an older quoted message) without the need to load the entire conversation.
- **Enhanced search experience:** Search results will be more useful by fetching both the matching messages and their surrounding context, allowing users to follow the flow of the conversation.

At the same time, this system will:
- Optimize memory usage.
- Contain database fetch operations.
- Maintain smooth scrolling.

### Option 1
Keep single "Fragment" of chat items

### Option 2
Keep 2 "Fragements" of chat items

### Option 3
Keep N "Fragments" of chat items