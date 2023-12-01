# Paginated chat list

## Problem

The UI is requesting a fresh chat list when switching profiles.
The `getChatPreviews` operation consists of a multiple DB queries fetching everything to show every item in the chat list.
Together, they produce quite a bit of data for every contact the user ever had.
This induces heavy load to bring up the data, marshall everything to UI and present it.
The cost grows with time, starting from negligible, but becoming a massive drain and producing a noticable latency spike.
Users who have many active profiles with constant traffic in all of them have their experience worsening despite having only a portion of contacts active in the momemnt.

A simple "give next N after M" pagination is not enough here as chat list elements may break order by jumping to top when an `newChatItem` or alike message arrives.

## Solution

UI should turn to lazy list containers and pagination to delay loading yet invisible elements.
To operate in this mode it needs to know a total amount of elements and the elements should have persistent IDs, unified across all chat types.

A proposed API prepares a "spine" of such a list, to be paginated and loaded lazilly.
Then, an item loader can request individual list elements that happen to enter the view.

The current API element (of type `Chat`) contains a triple of `ChatInfo c`/`[CChatItem c]`/`ChatStats`.
For lazy loading only a chat reference (composed from the `chatInfo.type` and an appropriate numeric ID) is needed.
This is enough to populate list element with item proxies and inflate visible elements to a full `Chat` object.
Incoming update events then can trigger their usual re-ordering, which may in turn trigger element update, if it comes into view.

> `CRNewChatItem` only has `AChatItem` data, so it can resolve a proxy partially.
> A call for item details may bring in missing data like `ChatInfo` and `ChatStats` later or immediately.

So, the new API is two commands and no new types.

- `APIGetChatList -> CRChatList [ChatRef]`
- `APIGetChatDetails ChatRef -> CRChatDetails Chat`

The UI should distinguish proxy elements containing only references from full elements and use platform APIs to request details when a proxy comes into view.
