//
//  ReverseList.swift
//  SimpleX (iOS)
//
//  Created by Levitating Pineapple on 11/06/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import Combine
import SimpleXChat

enum ReverseListSection { case main }

/// A List, which displays it's items in reverse order - from bottom to top
struct ReverseList<Content: View>: UIViewControllerRepresentable {
    @Binding var mergedItems: MergedItems
    @Binding var revealedItems: Set<Int64>
    @Binding var unreadCount: Int

    @Binding var scrollState: ReverseListScrollModel.State
    @Binding var loadingMoreItems: Bool
    @Binding var allowLoadMoreItems: Bool
    @Binding var ignoreLoadingRequests: Int64?

    /// Closure, that returns user interface for a given item
    /// Index, merged item
    let content: (Int, MergedItem) -> Content

    // unchecked, pagination, visibleItemIndexesNonReversed
    let loadItems: (Bool, ChatPagination) async -> Bool

    func makeUIViewController(context: Context) -> Controller {
        Controller(representer: self)
    }

    func updateUIViewController(_ controller: Controller, context: Context) {
        logger.log("LALAL UPDATEUICONTROLLER")
        controller.representer = self
        if case let .scrollingTo(destination) = scrollState, !mergedItems.items.isEmpty {
            if !controller.scrollToItemInProgress {
                switch destination {
                case let .item(id):
                    let row = mergedItems.indexInParentItems[id]
                    if let row {
                        logger.debug("LALAL SCROLLING TO \(row)")
                        controller.scroll(to: row, position: .bottom)
                        controller.setScrollEndedListener()
                        logger.debug("LALAL SCROLLING ENDED TO \(row)")
                    } else {
                        //controller.scrollToItem(id)
                    }
                case let .row(row):
                    controller.scroll(to: row, position: .bottom)
                    controller.setScrollEndedListener()
                case .bottom:
                    DispatchQueue.main.async {
                        logger.log("LALAL SCROLL TO BOTTOM START")
                        controller.scrollToItemInProgress = true
                        controller.scroll(to: 0, position: .top)
                        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                            controller.setScrollEndedListener()
                            controller.scrollToItemInProgress = false
                            logger.log("LALAL SCROLL TO BOTTOM END \(controller.tableView.contentOffset.y)")
                        }
                    }
                }
            }
        } else {
            // when tableView is dragging and new items are added, scroll position cannot be set correctly
            // so it's better to just wait until dragging ends
            if !controller.useSmoothScrolling && !mergedItems.splits.isEmpty && (controller.tableView.isDragging || controller.updatingInProgress || controller.scrollToItemInProgress)/* && !controller.tableView.isDecelerating*/ {
                controller.runBlockOnEndDecelerating = {
                    controller.runBlockOnEndDecelerating = nil
                    Task {
                        let mergedItems = $mergedItems.wrappedValue
                        let prevSnapshot = controller.prevSnapshot
                        logger.debug("LALAL SAME ITEMS, not rebuilding the tableview0")
                        if mergedItems.items == prevSnapshot.itemIdentifiers {
                            logger.debug("LALAL SAME ITEMS, not rebuilding the tableview1")
                            // update counters because they are static, unbound to specific chat and will become outdated if a new empty chat was open after non-empty one with unread messages
                            //controller.updateFloatingButtons.send()
                            return
                        }
                        logger.debug("LALAL SNAPSHOT SIZE0 \(prevSnapshot.numberOfItems)")
                        await MainActor.run {
                            controller.update(mergedItems)
                        }
                    }
                }
            } else {
                controller.runBlockOnEndDecelerating = nil
                if controller.prevSnapshot.numberOfItems == 0 {
                    controller.update(mergedItems)
                } else {
                    Task {
                        let mergedItems = $mergedItems.wrappedValue
                        logger.debug("LALAL SAME ITEMS, not rebuilding the tableview2")
                        if mergedItems.items == controller.prevSnapshot.itemIdentifiers {
                            logger.debug("LALAL SAME ITEMS, not rebuilding the tableview3")
                            // update counters because they are static, unbound to specific chat and will become outdated if a new empty chat was open after non-empty one with unread messages
                            //controller.updateFloatingButtons.send()
                            return
                        }
                        await MainActor.run {
                            let prevSnapshot = controller.prevSnapshot
                            controller.update(mergedItems)
                        }
                    }
                }
            }
        }
    }

    /// Controller, which hosts SwiftUI cells
    public class Controller: UITableViewController {
        var representer: ReverseList
        var dataSource: UITableViewDiffableDataSource<ReverseListSection, MergedItem>!

        //
        var prevSnapshot: NSDiffableDataSourceSnapshot<ReverseListSection, MergedItem> = NSDiffableDataSourceSnapshot()
        var prevMergedItems: MergedItems = MergedItems(items: [], splits: [], indexInParentItems: [:])
        //let updateFloatingButtons = PassthroughSubject<Void, Never>()
        //private var bag = Set<AnyCancellable>()

        var runBlockOnEndDecelerating: (() -> Void)? = nil

        private var scrollToRowOnAppear = 0

        private var prevFirstVisible = -1
        private var prevItemsCount = -1

        // it's set when .around call will be executed which requires to insert more items and skip scrolling
        var scrollToItemInProgress = false

        private var runBlockOnEndScrolling: (() -> Void)? = nil

        let useSmoothScrolling = false
        var preloading = false

        var updatingInProgress = false

        init(representer: ReverseList) {
            self.representer = representer
            super.init(style: .plain)

            // 1. Style
            tableView = InvertedTableView()
            tableView.separatorStyle = .none
            tableView.transform = .verticalFlip
            tableView.backgroundColor = .clear

            // 2. Register cells
            if #available(iOS 16.0, *) {
                tableView.register(
                    UITableViewCell.self,
                    forCellReuseIdentifier: cellReuseId
                )
            } else {
                tableView.register(
                    HostingCell<Content>.self,
                    forCellReuseIdentifier: cellReuseId
                )
            }

            // 3. Configure data source
            self.dataSource = UITableViewDiffableDataSource<ReverseListSection, MergedItem>(
                tableView: tableView
            ) { (tableView, indexPath, item) -> UITableViewCell? in
                let cell = tableView.dequeueReusableCell(withIdentifier: cellReuseId, for: indexPath)
                if #available(iOS 16.0, *) {
                    cell.contentConfiguration = UIHostingConfiguration { self.representer.content(indexPath.item, item) }
                        .margins(.all, 0)
                        .minSize(height: 1) // Passing zero will result in system default of 44 points being used
                } else {
                    if let cell = cell as? HostingCell<Content> {
                        cell.set(content: self.representer.content(indexPath.item, item), parent: self)
                    } else {
                        fatalError("Unexpected Cell Type for: \(item)")
                    }
                }
                cell.transform = .verticalFlip
                cell.selectionStyle = .none
                cell.backgroundColor = .clear
                _ = logger.debug("LALAL ASK FOR CELL \(indexPath.row)")
                return cell
            }

//            updateFloatingButtons
//                .throttle(for: 0.2, scheduler: DispatchQueue.global(qos: .background), latest: true)
//                .sink {
//                    if !self.scrollToItemInProgress {
//                        let items = representer.$mergedItems.wrappedValue.items
//                        let listState = DispatchQueue.main.sync(execute: { [weak self] in self?.getListState() }) ?? ListState()
//                        //ChatView.FloatingButtonModel.shared.updateOnListChange(items, listState)
//                        logger.debug("LALAL UPDATE FLOATING WITH ITEMS \(items.count)")
//                    }
//                }
//                .store(in: &bag)
        }

        @available(*, unavailable)
        required init?(coder: NSCoder) { fatalError() }

        deinit { NotificationCenter.default.removeObserver(self) }

        /// Hides keyboard, when user begins to scroll.
        /// Equivalent to `.scrollDismissesKeyboard(.immediately)`
        override func scrollViewWillBeginDragging(_ scrollView: UIScrollView) {
            UIApplication.shared
                .sendAction(
                    #selector(UIResponder.resignFirstResponder),
                    to: nil,
                    from: nil,
                    for: nil
                )
            NotificationCenter.default.post(name: .chatViewWillBeginScrolling, object: nil)
        }

        override public func scrollViewDidEndDecelerating(_ scrollView: UIScrollView) {
            if !updatingInProgress {
                runBlockOnEndDecelerating?()
                runBlockOnEndDecelerating = nil
            }
        }

        /// depending on tableView layout phase conditions, it can already have known size or not. Not possible to correctly scroll to required
        /// item if the size is unknown
        func scrollToRowWhenKnowSize(_ row: Int) {
            //            logger.debug("LALAL WILL SCROLL TO \(self.scrollToRowOnAppear)")
            if row > 0 && tableView.visibleSize.height > 0 {
                //                logger.debug("LALAL OFFSET before \(self.tableView.contentOffset.y), visible \(self.tableView.visibleSize.height) \(self.tableView.frame.height) \(self.view.frame.height) \(self.tableView.indexPathsForVisibleRows!)")
                //tableView.setContentOffset(CGPointMake(0, tableView.contentOffset.y - tableView.visibleSize.height), animated: false)
                tableView.scrollToRow(at: IndexPath(item: min(tableView.numberOfRows(inSection: 0) - 1, row), section: 0), at: .bottom, animated: false)
                // Without this small scroll position is not correct pixel-to-pixel.
                // Only needed when viewDidAppear has not been called yet because in there clipsToBounds is applied
                if tableView.clipsToBounds {
                    //tableView.setContentOffset(CGPointMake(0, tableView.contentOffset.y - 5), animated: false)
                }
                //                logger.debug("LALAL OFFSET after \(self.tableView.contentOffset.y) \(self.tableView.indexPathsForVisibleRows!)")
                scrollToRowOnAppear = 0
            } else {
                scrollToRowOnAppear = row
            }
        }

        override public func viewIsAppearing(_ animated: Bool) {
            super.viewIsAppearing(animated)
            scrollToRowWhenKnowSize(scrollToRowOnAppear)
        }

        override func viewDidAppear(_ animated: Bool) {
            tableView.clipsToBounds = false
            parent?.viewIfLoaded?.clipsToBounds = false
        }

        /// Scrolls to Item at index path
        func scroll(to index: Int, position: UITableView.ScrollPosition) {
            var animated = false
            if #available(iOS 16.0, *) {
                animated = true
            }
            if prevSnapshot.numberOfItems > index && index >= 0 {
                tableView.scrollToRow(
                    at: IndexPath(row: index, section: 0),
                    at: position,
                    animated: animated
                )
            } else {
                logger.error("Scroll: index out of bounds of table view")
            }
        }

        func setScrollEndedListener() {
            // wait until scrolling finishes so other calls will not interrupt it until then
            let task = Task {
                do {
                    try await Task.sleep(nanoseconds: 2000_000000)
                    // in case of listener will not be called
                    await MainActor.run {
                        representer.scrollState = .atDestination
                        //self.preloadIfNeeded()
                    }
                } catch {}
            }
            runBlockOnEndScrolling = {
                task.cancel()
                self.representer.scrollState = .atDestination
                self.runBlockOnEndScrolling = nil
                //self.preloadIfNeeded()
            }
        }

        func update(_ mergedItems: MergedItems) {
            if updatingInProgress {
                return
            }
            updatingInProgress = true
            tableView.panGestureRecognizer.isEnabled = false
            let items: [MergedItem] = mergedItems.items
            let indexInParentItems = mergedItems.indexInParentItems
            let snapshot = NSDiffableDataSourceSnapshot<ReverseListSection, MergedItem>()

            logger.debug("LALAL STEP 1  \(items.count)")
            let wasCount = self.prevSnapshot.numberOfItems
            let willBeCount = items.count
            let c = 1
            let insertedSeveralNewestItems = wasCount != 0 && willBeCount - wasCount == c && prevSnapshot.itemIdentifiers.first!.hashValue == items[c].hashValue
            logger.debug("LALAL STEP 2")
            logger.debug("LALAL WAS \(wasCount) will be \(items.count)")
//            var snapshot = NSDiffableDataSourceSnapshot<ReverseListSection, MergedItem>()
//            snapshot.appendSections([.main])
//            snapshot.appendItems(items)
            logger.debug("LALAL STEP 3")
            dataSource.defaultRowAnimation = .none

            // Sets content offset on initial load
            logger.debug("LALAL STEP 4")
            if wasCount == 0 {
                prevSnapshot = snapshot
                prevMergedItems = mergedItems
                dataSource.apply(
                    snapshot,
                    animatingDifferences: insertedSeveralNewestItems
                )
                if let firstUnreadItem = snapshot.itemIdentifiers.lastIndex(where: { $0.hasUnread() }) {
                    scrollToRowWhenKnowSize(firstUnreadItem)
                } else {
                    tableView.setContentOffset(
                        CGPoint(x: 0, y: -InvertedTableView.inset),
                        animated: false
                    )
                }
                logger.debug("LALAL STEP 5 0")
                updatingInProgress = false
                tableView.panGestureRecognizer.isEnabled = true
            } else if wasCount != snapshot.numberOfItems {
                logger.debug("LALAL drag \(self.tableView.isDragging), decel \(self.tableView.isDecelerating)")
                if useSmoothScrolling && tableView.isDecelerating {
                    tableView.panGestureRecognizer.isEnabled = false
                    tableView.beginUpdates()
                    prevSnapshot = snapshot
                    prevMergedItems = mergedItems
                    dataSource.apply(
                        snapshot,
                        animatingDifferences: false
                    )
                    tableView.endUpdates()
                    tableView.panGestureRecognizer.isEnabled = true
                    logger.debug("LALAL STEP 5 1")
                    updatingInProgress = false
                } else {
                    // remember current translation
                    var translationToApply: CGPoint? = nil
                    if let superview = self.tableView.superview {
                        let t = self.tableView.panGestureRecognizer.translation(in: superview)
                        if t.y != 0 {
                            translationToApply = t
                        }
                    }
                    let listState = getListState()
                    let wasFirstVisibleOffset = listState?.firstVisibleItemOffset ?? 0
                    let wasFirstIndex = listState?.firstVisibleItemIndex
                    let wasFirstId: Int64? = if let wasFirstIndex, prevSnapshot.itemIdentifiers.count > wasFirstIndex { prevSnapshot.itemIdentifiers[wasFirstIndex].newest().item.id
                    } else {
                        nil
                    }
                    let nowFirstIndex: Int? = if let wasFirstId { indexInParentItems[wasFirstId] } else { nil }
//                    let wasRowY = tableView.superview?.convert(
//                        tableView.rectForRow(at: IndexPath(row: wasFirstIndex!, section: 0)),
//                        from: tableView
//                    ).maxY
                    //logger.debug("LALAL DIFF start")
                    //let diff = snapshot.itemIdentifiers.difference(from: prevSnapshot.itemIdentifiers)
                    //logger.debug("LALAL DIFFERENCES \(diff.insertions.count) \(diff.removals.count)")
                    self.prevSnapshot = snapshot
                    self.prevMergedItems = mergedItems
                    dataSource.apply(
                        snapshot,
                        animatingDifferences: false
                    )
                    let countDiff = if let wasFirstIndex, let nowFirstIndex {
                        nowFirstIndex - wasFirstIndex
                    } else { 0 }

                    logger.debug("LALAL NEARSPLIT split lower WAS LISTSTATE \(listState?.firstVisibleItemIndex ?? -3)  now \(self.getListState()?.firstVisibleItemIndex ?? -4)  countDiff \(countDiff)    wasFirstIndex \(wasFirstIndex ?? -5) nowIndex \(nowFirstIndex ?? -1)   wasFirstId \(wasFirstId ?? -1) count \(self.prevSnapshot.itemIdentifiers.count)")

                    if countDiff == 0 {
                        // added new items to top, nothing to do, scrolling position is correct
                    } else {
                        // added new items to bottom
                        logger.debug("LALAL BEFORE SCROLLTOROW \(snapshot.numberOfItems - 1)  \(countDiff)  \(wasFirstIndex ?? -5)  \(self.tableView.contentOffset.y)  \(wasFirstVisibleOffset)   \(String(describing: self.representer.mergedItems.splits))")
                        self.getListState()
//                        let currentRowY = tableView.superview?.convert(
//                            tableView.rectForRow(at: IndexPath(row: (max(0, min(snapshot.numberOfItems - 1, countDiff + (wasFirstIndex ?? 0)))), section: 0)),
//                            from: tableView
//                        ).maxY
//                        if let wasRowY, let currentRowY {
//                            //self.tableView.setContentOffset(
//                            //    CGPoint(x: 0, y: self.tableView.contentOffset.y + (wasRowY - currentRowY)),
//                            //    animated: false
//                            //)
//                            logger.debug("LALAL WAS ROW \(wasRowY) \(currentRowY)")
//                        } else {
//                            logger.debug("LALAL WAS ROW \(wasRowY ?? -1) \(currentRowY ?? -2)")
//                        }
                        self.tableView.scrollToRow(
                            at: IndexPath(row: max(0, min(snapshot.numberOfItems - 1, countDiff + (wasFirstIndex ?? 0))), section: 0),
                            at: .top,
                            animated: false
                        )
                        self.tableView.setContentOffset(
                            CGPoint(x: 0, y: self.tableView.contentOffset.y - wasFirstVisibleOffset),
                            animated: false
                        )
                        self.tableView.layoutIfNeeded()
                        //logger.debug("LALAL AFTER SCROLLTOROW")
                        //let state = self.getListState()!
                        //logger.debug("LALAL NOW FIRST VISIBLE \(state.firstVisibleItemIndex) \(state.firstVisibleItemOffset)")

                        if let t = translationToApply {
                            //                        self.tableView.panGestureRecognizer.setTranslation(CGPointMake(t.x, t.y), in: self.tableView.superview!)
                            let o = self.tableView.contentOffset
                            //                        UIView.animate(withDuration: 0.5) {
                            //                            self.tableView.setContentOffset(CGPointMake(o.x + t.x, o.y + t.y), animated: false)
                            //                            self.tableView.layoutIfNeeded()
                            //                        }
                            self.tableView.setContentOffset(CGPointMake(o.x + t.x, o.y + t.y), animated: true)
                        }
                    }
                    logger.debug("LALAL STEP 5 2")
                    self.updatingInProgress = false
                    tableView.panGestureRecognizer.isEnabled = true
                }
            } else {
                prevSnapshot = snapshot
                prevMergedItems = mergedItems
                dataSource.apply(
                    snapshot,
                    animatingDifferences: false
                )
                logger.debug("LALAL STEP 5 3")
                updatingInProgress = false
                tableView.panGestureRecognizer.isEnabled = true
            }
            // covered edge case when you open a chat with a lot of groupped items which smaller than screen size
            //self.preloadIfNeeded()
            //updateFloatingButtons.send()
            logger.debug("LALAL STEP 6")
        }

        override func scrollViewDidScroll(_ scrollView: UIScrollView) {
//            updateFloatingButtons.send()
//
//            if representer.scrollState == .atDestination, let listState = self.getListState() {
//                if nearSplit(remaining: 30, ignoreTopOfTopSplit: true, listState, prevMergedItems) {
//                    //logger.debug("LALAL IN SPLIT OR NO YESSSSSS  \(listState.firstVisibleItemIndex)..\(listState.lastVisibleItemIndex)   \(String(describing: self.prevMergedItems.splits))")
//                    stopScrolling(disable: false)
//                }
//                //logger.debug("LALAL IN SPLIT OR NO \(self.nearSplit(remaining: 40, ignoreTopOfTopSplit: false, listState, self.prevMergedItems))  \(listState.firstVisibleItemIndex)..\(listState.lastVisibleItemIndex)   \(String(describing: self.prevMergedItems.splits))")
//                if let block = runBlockOnEndDecelerating, nearSplit(remaining: 40, ignoreTopOfTopSplit: false, listState, prevMergedItems) {
//                    if !updatingInProgress {
//                        runBlockOnEndDecelerating = nil
//                        // it's important to have it in DispatchQueue.main. Otherwise, it will be deadlock and jumping scroll
//                        // without any visible reason
//                        DispatchQueue.main.async {
//                            block()
//                        }
//                    }
//                } else {
//                    preloadIfNeeded()
//                }
//            }
        }

        override public func scrollViewDidEndScrollingAnimation(_ scrollView: UIScrollView) {
            if let onEnd = runBlockOnEndScrolling {
                DispatchQueue.main.async {
                    onEnd()
                }
            }
        }

        func stopScrolling(disable: Bool = false) {
            tableView.setContentOffset(self.tableView.contentOffset, animated: false)
            if disable {
                tableView.panGestureRecognizer.isEnabled = false
            }
        }

        func nearSplit(remaining: Int, ignoreTopOfTopSplit: Bool , _ listState: ListState, _ prevMergedItems: MergedItems) -> Bool {
            if prevMergedItems.splits.isEmpty { return false }

            let firstVisibleIndex = listState.firstVisibleItemIndex
            let lastVisibleIndex = listState.lastVisibleItemIndex
            for split in prevMergedItems.splits {
                // before any split
                //logger.debug("LALAL NEARSPLIT split lower \(split.indexRangeInParentItems.lowerBound)  last \(lastVisibleIndex) first \(firstVisibleIndex) remaining \(remaining)")
                if split.indexRangeInParentItems.lowerBound > lastVisibleIndex {
                    if lastVisibleIndex > (split.indexRangeInParentItems.lowerBound - remaining) {
                        return true
                    }
                    break
                }
                let containsInRange = split.indexRangeInParentItems.contains(firstVisibleIndex)
                if containsInRange {
                    if lastVisibleIndex > (split.indexRangeInParentItems.upperBound - remaining) {
                        // situation when there is nothing to load from top
                        if ignoreTopOfTopSplit && split.indexRangeInParentItems == prevMergedItems.splits.last?.indexRangeInParentItems {
                            return false
                        }
                        return true
                    }
                    if firstVisibleIndex < (split.indexRangeInParentItems.lowerBound + remaining) {
                        return true
                    }
                    break
                }
            }
            return false
        }

        func getListState() -> ListState? {
            if scrollToItemInProgress {
                return nil
            }
            logger.debug("LALAL VISIBLE ROWS \((self.tableView.indexPathsForVisibleRows ?? []).map({ $0.row }))")
            let items = prevSnapshot.itemIdentifiers
            if let visibleRows = tableView.indexPathsForVisibleRows,
               visibleRows.last?.row ?? 0 < items.count {
                let scrollOffset: Double = tableView.contentOffset.y + InvertedTableView.inset
                guard let firstVisible = visibleRows.first(where: { isVisible(indexPath: $0) }),
                      let lastVisible = visibleRows.last(where: { isVisible(indexPath: $0) }) else {
                    return nil
                }

                let topItemDate: Date? = items[lastVisible.item].oldest().item.meta.itemTs
                let firstVisibleOffset: CGFloat? = offsetForRow(firstVisible.row)
                logger.debug("LALAL LAST \(lastVisible.item)")
                let bottomItemId: ChatItem.ID = items[firstVisible.row].newest().item.id
                return ListState(scrollOffset: scrollOffset, topItemDate: topItemDate, bottomItemId: bottomItemId, firstVisibleItemIndex: firstVisible.row, lastVisibleItemIndex: lastVisible.row, firstVisibleItemOffset: firstVisibleOffset ?? 0)
            }
            return nil
        }

        private func offsetForRow(_ row: Int) -> CGFloat? {
            if let relativeFrame = tableView.superview?.convert(
                tableView.rectForRow(at: IndexPath(row: row, section: 0)),
                from: tableView
            ),  relativeFrame.maxY > InvertedTableView.inset &&
                relativeFrame.minY < tableView.frame.height - InvertedTableView.inset {
                // it is visible
                let offset = tableView.frame.height - InvertedTableView.inset - relativeFrame.maxY
                logger.debug("LALAL ROW \(row) minY \(relativeFrame.minY) maxY \(relativeFrame.maxY) table \(self.tableView.frame.height) inset \(InvertedTableView.inset)")
                return offset
            } else { return nil }
        }

        private func isVisible(indexPath: IndexPath) -> Bool {
            if let relativeFrame = tableView.superview?.convert(
                tableView.rectForRow(at: indexPath),
                from: tableView
            ) {
                relativeFrame.maxY > InvertedTableView.inset &&
                relativeFrame.minY < tableView.frame.height - InvertedTableView.inset
            } else { false }
        }

//        func scrollToItem(_ itemId: Int64) {
//            if scrollToItemInProgress {
//                logger.debug("LALAL SCROLLING IN PROGRESS")
//                return
//            }
//            scrollToItemInProgress = true
//            Task {
//                logger.debug("LALAL SCROLL TO ITEM \(itemId)  \(ItemsModel.shared.reversedChatItems.count)")
//                do {
//                    var index = representer.mergedItems.indexInParentItems[itemId]
//                    // setting it to 'loading' even if the item is loaded because in rare cases when the resulting item is near the top, scrolling to
//                    // it will trigger loading more items and will scroll to incorrect position (because of trimming)
//                    await MainActor.run {
//                        representer.loadingMoreItems = true
//                    }
//                    logger.debug("LALAL SCROLL TO ITEM \(index ?? -2)")
//                    if index == nil {
//                        let pagination = ChatPagination.around(chatItemId: itemId, count: ChatPagination.PRELOAD_COUNT * 2)
//                        let oldSize = ItemsModel.shared.reversedChatItems.count
//                        _ = await self.representer.loadItems(true, pagination, { self.visibleItemIndexesNonReversed(self.prevMergedItems)})
//                        var repeatsLeft = 50
//                        while oldSize == ItemsModel.shared.reversedChatItems.count && repeatsLeft > 0 {
//                            try await Task.sleep(nanoseconds: 20_000000)
//                            repeatsLeft -= 1
//                            logger.debug("LALAL SCROLL REPEATS \(repeatsLeft)")
//                        }
//                        index = representer.mergedItems.indexInParentItems[itemId]
//                    }
//                    logger.debug("LALAL SCROLL TO ITEM2 \(index ?? -3) \(ItemsModel.shared.reversedChatItems.count)")
//                    if let index {
//                        scroll(to: min(ItemsModel.shared.reversedChatItems.count - 1, index), position: .bottom)
//                    } else {
//                        await MainActor.run {
//                            representer.scrollState = .atDestination
//                        }
//                    }
//                } catch {
//                    logger.error("Error scrolling to item: \(error)")
//                }
//                await MainActor.run {
//                    representer.loadingMoreItems = false
//                    scrollToItemInProgress = false
//                }
//            }
//        }

//        func preloadIfNeeded() {
//            let mergedItems = self.representer.mergedItems
//            guard let state = self.getListState(),
//                  representer.scrollState == .atDestination,
//                  prevFirstVisible != state.firstVisibleItemIndex || prevItemsCount != mergedItems.indexInParentItems.count,
//                  !preloading
//            else {
//                return
//            }
//            //logger.debug("LALAL LOADING BEFORE ANYTHING \(state.firstVisibleItemIndex) \(self.prevSnapshot.itemIdentifiers[state.firstVisibleItemIndex].newest().item.id)  \(self.representer.$mergedItems.wrappedValue.items[state.firstVisibleItemIndex].newest().item.id)")
//            prevFirstVisible = state.firstVisibleItemIndex
//            prevItemsCount = mergedItems.indexInParentItems.count
//            preloading = true
//            Task {
//                defer {
//                    preloading = false
//                }
//                //logger.debug("LALAL LOADING BEFORE INSIDE \(state.firstVisibleItemIndex) \(self.prevSnapshot.itemIdentifiers[state.firstVisibleItemIndex].newest().item.id)  \(mergedItems.items[state.firstVisibleItemIndex].newest().item.id) \(mergedItems.splits)")
//                await preloadItems(mergedItems, self.representer.allowLoadMoreItems, state, self.representer.$ignoreLoadingRequests) { pagination in
//                    let triedToLoad = await self.representer.loadItems(false, pagination, { self.visibleItemIndexesNonReversed(self.prevMergedItems) })
//                    logger.debug("LALAL LOADING INSIDE \(mergedItems.items[state.firstVisibleItemIndex].newest().item.id) \(mergedItems.splits), triedToLoad: \(triedToLoad)")
//                    return triedToLoad
//                }
//            }
//        }
    }

    /// `UIHostingConfiguration` back-port for iOS14 and iOS15
    /// Implemented as a `UITableViewCell` that wraps and manages a generic `UIHostingController`
//    private final class HostingCell<Hosted: View>: UITableViewCell {
//        private let hostingController = UIHostingController<Hosted?>(rootView: nil)
//
//        /// Updates content of the cell
//        /// For reference: https://noahgilmore.com/blog/swiftui-self-sizing-cells/
//        func set(content: Hosted, parent: UIViewController) {
//            hostingController.view.backgroundColor = .clear
//            hostingController.rootView = content
//            if let hostingView = hostingController.view {
//                hostingView.invalidateIntrinsicContentSize()
//                if hostingController.parent != parent { parent.addChild(hostingController) }
//                if !contentView.subviews.contains(hostingController.view) {
//                    contentView.addSubview(hostingController.view)
//                    hostingView.translatesAutoresizingMaskIntoConstraints = false
//                    NSLayoutConstraint.activate([
//                        hostingView.leadingAnchor
//                            .constraint(equalTo: contentView.leadingAnchor),
//                        hostingView.trailingAnchor
//                            .constraint(equalTo: contentView.trailingAnchor),
//                        hostingView.topAnchor
//                            .constraint(equalTo: contentView.topAnchor),
//                        hostingView.bottomAnchor
//                            .constraint(equalTo: contentView.bottomAnchor)
//                    ])
//                }
//                if hostingController.parent != parent { hostingController.didMove(toParent: parent) }
//            } else {
//                fatalError("Hosting View not loaded \(hostingController)")
//            }
//        }
//
//        override func prepareForReuse() {
//            super.prepareForReuse()
//            hostingController.rootView = nil
//        }
//    }
}

class ListState {
    let scrollOffset: Double
    let topItemDate: Date?
    let bottomItemId: ChatItem.ID?
    let firstVisibleItemIndex: Int
    let lastVisibleItemIndex: Int
    let firstVisibleItemOffset: CGFloat // can be negative or zero

    init(scrollOffset: Double = 0, topItemDate: Date? = nil, bottomItemId: ChatItem.ID? = nil, firstVisibleItemIndex: Int = 0, lastVisibleItemIndex: Int = 0, firstVisibleItemOffset: CGFloat = 0) {
        self.scrollOffset = scrollOffset
        self.topItemDate = topItemDate
        self.bottomItemId = bottomItemId
        self.firstVisibleItemIndex = firstVisibleItemIndex
        self.lastVisibleItemIndex = lastVisibleItemIndex
        self.firstVisibleItemOffset = firstVisibleItemOffset
    }
}

class ReverseListScrollModel: ObservableObject {
    var scrollView: EndlessScrollView<MergedItem>!
    var mergedItems: BoxedValue<MergedItems>!

    var loadChatItems: ((ChatPagination) async -> Bool)!

    enum State: Equatable {
        enum Destination: Equatable {
            case item(ChatItem.ID)
            case row(Int)
            case bottom
        }

        case scrollingTo(Destination)
        case atDestination
    }

    @Published var state: State = .atDestination

    func scrollToBottom() {
        Task {
            logger.log("LALAL SCROLL TO BOTTOM START")
            //controller.scrollToItemInProgress = true
            await scrollView.scrollToItem(0, animated: true, top: false)
            //DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
            //  controller.setScrollEndedListener()
            //controller.scrollToItemInProgress = false
            //logger.log("LALAL SCROLL TO BOTTOM END \(controller.tableView.contentOffset.y)")
            //}
        }
    }

    func scrollToItem(itemId: ChatItem.ID) {
//        if scrollToItemInProgress {
//            logger.debug("LALAL SCROLLING IN PROGRESS")
//            return
//        }
//        scrollToItemInProgress = true
        Task {
            logger.debug("LALAL SCROLL TO ITEM \(itemId)  \(ItemsModel.shared.reversedChatItems.count)")
            do {
                var index = mergedItems.boxedValue.indexInParentItems[itemId]
                logger.debug("LALAL SCROLL TO ITEM \(index ?? -2)")
                if index == nil {
                    let pagination = ChatPagination.around(chatItemId: itemId, count: ChatPagination.PRELOAD_COUNT * 2)
                    let oldSize = ItemsModel.shared.reversedChatItems.count
                    let triedToLoad = await self.loadChatItems(pagination)
                    if !triedToLoad {
                        return
                    }
                    var repeatsLeft = 50
                    while oldSize == ItemsModel.shared.reversedChatItems.count && repeatsLeft > 0 {
                        try await Task.sleep(nanoseconds: 20_000000)
                        repeatsLeft -= 1
                        logger.debug("LALAL SCROLL REPEATS \(repeatsLeft)")
                    }
                    index = mergedItems.boxedValue.indexInParentItems[itemId]
                }
                logger.debug("LALAL SCROLL TO ITEM2 \(index ?? -3) \(ItemsModel.shared.reversedChatItems.count)")
                if let index {
                    scrollToItem(index: min(ItemsModel.shared.reversedChatItems.count - 1, index))
                }
            } catch {
                logger.error("Error scrolling to item: \(error)")
            }
//            await MainActor.run {
//                representer.loadingMoreItems = false
//                scrollToItemInProgress = false
//            }
        }
    }

    func scrollToItem(index: Int) {
        Task {
            await scrollView.scrollToItem(index, animated: true)
        }
        //controller.setScrollEndedListener()
    }

    func scroll(by: CGFloat, animated: Bool = true) {
        scrollView.setContentOffset(CGPointMake(scrollView.contentOffset.x, scrollView.contentOffset.y + by), animated: animated)
    }
}

fileprivate let cellReuseId = "hostingCell"

fileprivate extension CGAffineTransform {
    /// Transform that vertically flips the view, preserving it's location
    static let verticalFlip = CGAffineTransform(scaleX: 1, y: -1)
}

/// Disable animation on iOS 15
func withConditionalAnimation<Result>(
    _ animation: Animation? = .default,
    _ body: () throws -> Result
) rethrows -> Result {
    if #available(iOS 16.0, *) {
        try withAnimation(animation, body)
    } else {
        try body()
    }
}

class InvertedTableView: UITableView {
    static let inset = CGFloat(100)

    static let insets = UIEdgeInsets(
        top: inset,
        left: .zero,
        bottom: inset,
        right: .zero
    )

    override var contentInsetAdjustmentBehavior: UIScrollView.ContentInsetAdjustmentBehavior {
        get { .never }
        set { }
    }

    override var contentInset: UIEdgeInsets {
        get { Self.insets }
        set { }
    }

    override var adjustedContentInset: UIEdgeInsets {
        Self.insets
    }

    override var contentOffset: CGPoint {
        get { super.contentOffset }
        set {
            logger.debug("LALAL SET OFFSET \(newValue.y)")
            super.contentOffset = newValue
        }
    }

    override func layoutSubviews() {
        let offset = contentOffset
        super.layoutSubviews()
        if offset != contentOffset {
            contentOffset = offset
        }
    }

    override func setContentOffset(_ contentOffset: CGPoint, animated: Bool) {
        logger.debug("LALAL SET OFFSET ANIMATED \(contentOffset.y)")
        super.setContentOffset(contentOffset, animated: animated)
    }

}
