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

### Pagination integration:

```
fetch(scrollingUp, chatItemId)
    if scrollingUp
        fetchBefore(chatItemId)
    else
        fetchAfter(chatItemId)

-- Another option would be if the api exposed a way to do before and after, possibly already supported since they are just separate query strings
fetchNearItem(chatItemId)
    fetchBefore(chatItemId)
    fetchAfter(chatItemId)

fetchLatest()
    fetchLast()
```

### Option 1
Keep single instance of `chatItems` representing the current scrollable chat area. The entire array is cleared and replaced during long-distance navigation, and items are appended or prepended as needed during scroll.

#### Data structures

```
chatItems = Array<ChatItem>
```

#### Behaviour

##### Scroll data fetching
```
if scrollingUp
    newItems = fetch(ScrollingUp, chatItems.first().id)
    chatItems.addToBeginning(newItems)
else
    newItems = fetch(ScrollingDown, chatItems.last().id)
    chatItems.addToEnd(newItems)
```

##### Long Distance Navigation
```
newItems = 
    if item
        fetchNearItem(item)
    else
        fetchLatest
chatItems = newItems -- Totally clears chat items
```

##### Requires fetch
Remains mainly unchanged, only needs to take into account direction and either use first or last item instead of always using first.

**Pros:**
- Simplest solution as it maintains only one instance of chat data, adding new data to the beginning or end of the array is straightforward, making this approach easy to implement

**Cons:**
- When jumping to the latest messages, especially for large databases, there will be a delay caused by the database fetch. For users with bigger DBs, this could degrade user experience significantly.
- Not reusing any of the previously loaded data means that the system performs extra database accesses when fetching items that have already been loaded.

### Option 2
2 sections within `chatItems` representing the current scrollable chat area and the bottom area. The bottom section is never cleared, while long distance navigation creates/rebuilds the anchor and resets items previously added on this section. Anchor is also destroyed in the cases where both sections intersect 

#### Data structures

```
chatItems = Array<ChatItem>

data ScrollAnchor {
    chatItemId: Long
    chatItemIndex: Int
}

// Optional Set for bottom section IDs
// Will take o(n) space where n is the number of items in the bottom section, It will make dedup and intersection operations o(1), that is probably a acceptable tradeoff as we want to minimize execution speed while scrolling.

Set<Long> bottomSectionIds
```

#### Behaviour

##### Scroll data fetching
```
if scrollingUp
    newItems = fetch(ScrollingUp, chatItems.first().id)
    if anchor
        if newItems intercepts items
            dedup newItems
            anchor = null
        else
            anchor.chatItemIndex += newItems.size
            anchor.chatItemId = newItems.last()

    chatItems.addAtIndex(0, newItems)
else
    if anchor
        newItems = fetch(ScrollingDown, anchor.chatItemId)
        if newItems intercepts items
            dedup newItems
            chatItems.insertAfter(anchor.chatItemIndex, newItems)
            anchor = null
        else
            chatItems.insertAfter(anchor.chatItemIndex, newItems)
            anchor.chatItemIndex += newItems.size
            anchor.chatItemId = newItems.last()
    else
        Noop -- never happens
```

##### Long Distance Navigation
```
if item
    newItems = fetchNearItem(item)
    if newItems intercepts items
        dedup newItems
        chatItems.insertAtStart(newItems)
        anchor = null
    else
        anchor = { chatItemId: newItems.last(), chatItemIndex: newItems.lastIndex }
        chatItems.insertAtStart(newItems)
else
    if anchor
        chatItems.remove(from: 0, to: anchor.chatItemIndex)
        anchor = null
```

##### Requires fetch
```
if scrollingUp
    -- Unchanged
else
    if anchor
        is diff between the index of bottom visible item and anchor chatItemIndex < PRELOAD value
    else
        false
```

**Pros:**
- Keeping two sections makes it simple for users to scroll through messages and jump to recent ones and doesn't add extreme complexity in code design.
- Most common action for users (going back to the bottom of the chat) are totally addressed and instant without exponetially increasing the ammount of data stored in device runtime memory.

**Cons:**
- Managing two sections can make the code harder to maintain and could introduce bugs if not handled carefully.
- Not reusing anchors means the app might have to access the database more often.
- Deduplication of items as they are fetched becomes of extreme importance as not doing it effectively can lead to duplicated messages being displayed.

### Option 3
N sections each with it's own instance of `chatItems` representing all previously fetched scrollable chat areas. Long distance navigation creates new anchors in cases date wasn't yet fetched. Sections are merged in cases where 2 section intercept.

#### Data structures

```
data ScrollSection {
    chatItems: Array<ChatItem>
    active: Bool
    bottom: Bool
}

scrollSections = Array<ScrollSection>

// Will take o(n) space where n is the number of items loaded, It will make dedup, merge and intersection operations o(1), that is probably a acceptable tradeoff as we want to minimize execution speed while scrolling.
chatIdSection = Map<Long = Chat Item Id, Int = Index In Scroll Sections>

activeSection = scrollSections.first { it.active }
chatItems = activeSection.chatItems
```

#### Behaviour

##### Scroll data fetching
```
if scrollingUp
    newItems = fetch(ScrollingUp, activeSection.chatItems.first().id)
    if chatIdSection has newItems.any
        mergeSectionsIntoActiveSection()
    activeSection.chatItems.addAtIndex(0, newItems)
else
    newItems = fetch(ScrollingUp, activeSection.chatItems.last().id)
    if chatIdSection has newItems.any
        mergeSectionsIntoActiveSection()
    activeSection.chatItems.insertAtEnd(newItems)
```

##### Long Distance Navigation
```
if item
    if item.id in chatIdSection
        swapActiveTo(scrollSection[chatIdSection[item.id]])
    else
        newSection = scrollSections.add(ScrollSection(
            chatItems: fetchNearItem(item)
            active: true
            bottom: false
        ))
        swapActiveTo(newSection)
else
    swapActiveTo(scrollSections.first { it.bottom })
```

##### Requires fetch
```
if scrollingUp
    -- Same as current with active section
else
    -- Inverted to current with active section
```

**Pros:**
- Least stresfull option database-wise.
- Most common action for users (going back to the bottom of the chat) are totally addressed.
- Any previosuly fetched data will be available to jump to without latency

**Cons:**
- This is the most complex solution engineering-wise
- The logic to merge sections can be computationally extensive and a place for errors to emerge due to it's complexity
- The ammount of runtime memory consumed by this solution will increase exponentially
