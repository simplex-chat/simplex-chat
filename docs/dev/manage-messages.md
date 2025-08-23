# Messages in UI

In this guide we will walk through how messages are managed in UI code of Android/desktop and iOS applications.

There are several files that load them from backend and process:
- `ChatItemsLoader.{kt,swift}`
- `ChatItemsMerger.{kt,swift}`
- `ChatView.{kt,swift}`
- `ChatScrollHelpers.swift` iOS-only
- `EndlessScrollView.swift` iOS-only.

## ChatItemsLoader

It plays an important role of loading items from backend and to structure them based on pagination type. Each pagination type has it's own structure. For example:
- when loading pagination with .initial type, the whole data in the view should be replaced with loaded items
- .around type requires all loaded results to be put into it's sorted position of all loaded items
- .last type expects all items to be added to the end of previously loaded data set
- .before/.after types require all items to be added near their requested item ID (the ID was sent with the pagination parameters).

The file also removes duplicates that can happen if some of the items were pushed by API before in events. And modifies splits in case they become larger or some split was merged with another one. And trims unused items to keep memory free when user scrolls up.

It manages data set in expectation that items are in their natural order from oldest to newest, not vice versa. The same way as API gives them (unlike in ChatView where they will become reversed).

### Splits

`Splits` is a concept of a range of items that is represented as a `Long`/`Int64` value equal to an item ID. For example, this is possible data set and splits array for it:
- `[{id: 0, ...}, {id: 1, ...}, {id: 2, ...}, {id: 4, ...}, {id: 5, ...}]` - just 5 items with IDs in ascending order loaded and already processed by `ChatItemsLoader` (but you can't rely on ascending order, this is just an example)
- `[2]` - this is `splits` array, which tells us that after item with id == `2` (including it) there is a split range with item IDs `[0, 1, 2]`. So, `[4, 5]` without any splits which means they represent bottom loaded part of the chat (and NOTHING is below them in database). Everything starting from splits IS NOT bottom items and should be treated differently than bottom items.

There may be many splits. Not just one. Example of such data set:
- `[{id: 0, ...}, {id: 1, ...}, {id: 3, ...}, {id: 4, ...}, {id: 6, ...}, {id: 7, ...}]` - data set
- `[1, 4]` - splits.

In this case there are two split ranges `[[0, 1], [3, 4]]` and bottom items `[6, 7]`. As you may notice, splits have sorting based on sorting of their items in data set. So you cannot have `[4, 1]` in this example, only `[1, 4]`.

#### How to work with splits

If you need to load data, you need to think which item ID to send to the API. If the idea is to load something INSIDE split range and BEFORE specific item, you need to pass item ID that is inside of the split range and put resulting items in their place based on ordering (for direct and group chats the ordering is different). You can't rely on ordering of item IDs. But if you query items .before existing item, you would put results on top of that item. If you query .after, you can put them after that item in data set.

Query .around adds complexity because you don't know where to put the items you loaded (for example, you need to show quoted item). In this case ordering by `created_at`/`item_ts` + `itemID` will help to decide exact position in the data set (see `indexToInsertAround()`).

## ChatItemsMerger

The purpose of this file is to create data set for presenting in `ChatView` and map items with additional details needed to be used inside views. There is a function called `MergedItems.create()` which processes the whole list of items in one loop. Mapping should be as fast as possible because this function will be called again after ANY change in underlying data set (eg., `reversedChatItems`). Note that unlike `ChatItemsLoader`, merger works with reversed items because the items you see in `ChatView` are in reversed order and this way identical IDs are being mapped in the merged items as well.

Merger produce a list of `items: [MergedItem]` which contains single or grouped item (many items can be grouped because they are deleted/moderated/events). So `items.size` == `visible items size in ChatView` always. When some items become ungrouped, they will be presented as single items in `items` array (note that any group can be expanded, which produce same number of single items as number of grouped items in one before).

There is a mapping `item ID -> index in items[]` so you can find exact place in `items` array without iterating over all items.

`MergedItems` also contains `splits` but represented differently: they are stored as ranges in reversed/parent items. Which helps to identify where specific item is, in which range it is or it isn't in any range. By saying `parent` item I mean the item that you can see on screen (single or grouped items into one). Example:
- `[0, 1, 2, 3, 4]` - default sorting of items (sorted from old to new, as API returns). Stored like this in `ChatModel.chatItems` (on Android/desktop)
- `[4, 3, 2, 1, 0]` - reversed items. Stored like this in `ItemsModel` (on iOS) and in `ChatView` (on Android/desktop)
- `[4, 3, 2]` - parent items - something that is visible on screen. So `items` array in `MergedItems` would show this: `[.single 4, .single 3, .grouped [2, 1, 0]]`.

## ChatView

The main idea of it is to present items in correct order. So .single items are presented as a single item, but .grouped items can be expanded/collapsed which requires to rebuild the data set and to remap items to their new structure. Remember, mapping of `MergedItems.items` should be the same as mapping of visible items. `ChatView` also reacts on scrolling of underlying component, eg. `LazyColumn` on Android/desktop and `EndlessScrollView` on iOS (on iOS `ChatScrollHelpers` helps in this case because it's easier to understand what happens there instead of putting everything in `ChatView`).

If `ChatView` will need some derived data from items, put it into `MergedItems.create()` loop only if putting it in `ChatView` slows down the view generation (for example, requires looping over data set).

## EndlessScrollView

That component was built to achieve the same scrolling possibilities that `LazyColumn` provides on Android/desktop. The features it provides:
- lazy loading of items that are presented on screen (no off-screen items at all)
- attaching logic of drawing the items based on `firstVisibleItemId`
- has `visible items` and `all items` arrays that were used to place views on screen in the last relayout
- can scroll to any item animated/non-animated to top/bottom side of it
- has overscroll effect support
- has scroll bar support which is based on unknown sizes of non-placed items and known visible items size.

This component was built as a module/plugin which doesn't have tied connection to other parts of codebase - it's self-contained. When you modify it, preserve this idea because it will allow us to use it in different places of the app. Less coupling makes it more pure and reduces possibility of bugs.

There are only three methods that are mainly used from this class:
- `updateItems()`
- `scrollToItem()`
- `scrollToItemAnimated() async`.

Everything else is less important. So calling these methods is enough for every possible use-case currenly.

Items placing is done in `adaptItems()` which tries to maintain position of `firstVisibleItemIndex` with `firstVisibleItemOffset` while covering a screen with items. It means that inserting elements on top or bottom of the visible area does nothing for visible items - no heavy reload of data as with `UITableView`/`UICollectionView`.

Adding items into visible list of items pushes them from bottom to top (if the first visible item was unaffected by insertation).

Scrolling to an item without animation is done in one graphic frame, so there is no shaking or visible position change before showing the final result of scroll. Animated scrolling speed can be adjusted and it's equal to scroll to top and scroll to bottom (nearly identical visually).

There is a possibility to attach a listener to changes of position of items or data set. The listener will be called after every `adaptItems()` call.
