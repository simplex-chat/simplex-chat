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

/// A List, which displays it's items in reverse order - from bottom to top
struct ReverseList<Content: View>: UIViewControllerRepresentable {
    @Binding var mergedItems: MergedItems
    @Binding var revealedItems: Set<Int64>
    @Binding var unreadCount: Int

    @Binding var scrollState: ReverseListScrollModel.State
    @Binding var loadingMoreItems: Bool
    @Binding var allowLoadMoreItems: Bool
    @Binding var ignoreLoadingRequests: [Int64]

    /// Closure, that returns user interface for a given item
    /// Index, merged item
    let content: (Int, MergedItem) -> Content

    // unchecked, pagination, visibleItemIndexesNonReversed
    let loadItems: (Bool, ChatPagination, @escaping @MainActor () -> ClosedRange<Int>) async -> Bool

    func makeUIViewController(context: Context) -> Controller {
        Controller(representer: self)
    }

    func updateUIViewController(_ controller: Controller, context: Context) {
        controller.representer = self
        if case let .scrollingTo(destination) = scrollState, !mergedItems.items.isEmpty, !controller.scrollToItemInProgress {
            controller.view.layer.removeAllAnimations()
            switch destination {
            case let .item(id):
                let row = mergedItems.indexInParentItems[id]
                if let row {
                    logger.debug("LALAL SCROLLING TO \(row)")
                    controller.scroll(to: row, position: .bottom)
                    logger.debug("LALAL SCROLLING ENDED TO \(row)")
                } else {
                    controller.scrollToItem(id)
                }
            case let .row(row):
                controller.scroll(to: row, position: .bottom)
            case .bottom:
                logger.debug("LALAL WANT TO SCROLL BOTTOM \(self.loadingMoreItems)")
                //DispatchQueue.main.async {
                    controller.scroll(to: 0, position: .top)
                    logger.debug("LALAL FINISHED SCROLL BOTTOM0")
                //}
                //logger.debug("LALAL FINISHED SCROLL BOTTOM1")
            }
        } else {
            // when tableView is dragging and new items are added, scroll position cannot be set correctly
            // so it's better to just wait until dragging ends
            if !mergedItems.splits.isEmpty && controller.tableView.isDragging/* && !controller.tableView.isDecelerating*/ {
                controller.runBlockOnEndDecelerating = {
                    controller.update(mergedItems.items)
                }
            } else {
                controller.runBlockOnEndDecelerating = nil
                controller.update(mergedItems.items)
            }
        }
    }

    /// Controller, which hosts SwiftUI cells
    public class Controller: UITableViewController {
        private enum Section { case main }
        var representer: ReverseList
        private var dataSource: UITableViewDiffableDataSource<Section, MergedItem>!
        private let updateFloatingButtons = PassthroughSubject<Void, Never>()
        private var bag = Set<AnyCancellable>()

        var runBlockOnEndDecelerating: (() -> Void)? = nil

        private var scrollToRowOnAppear = 0

        private var prevFirstVisible = -1

        // it's set when .around call will be executed which requires to insert more items and skip scrolling
        var scrollToItemInProgress = false

        private var runBlockOnEndScrolling: (() -> Void)? = nil

        private let useSmoothScrolling = false

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
            self.dataSource = UITableViewDiffableDataSource<Section, MergedItem>(
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
                return cell
            }

            // 4. External state changes will require manual layout updates
            NotificationCenter.default
                .addObserver(
                    self,
                    selector: #selector(updateLayout),
                    name: notificationName,
                    object: nil
                )

            updateFloatingButtons
                .throttle(for: 0.2, scheduler: DispatchQueue.global(qos: .background), latest: true)
                .sink {
                    let items = representer.mergedItems.items
                    let listState = DispatchQueue.main.sync(execute: { [weak self] in self?.getListState(items) }) ?? ListState()
                    ChatView.FloatingButtonModel.shared.updateOnListChange(items, listState)
                    //self.preloadIfNeeded(listState)
                }
                .store(in: &bag)
        }

        @available(*, unavailable)
        required init?(coder: NSCoder) { fatalError() }

        deinit { NotificationCenter.default.removeObserver(self) }

        @objc private func updateLayout() {
            if #available(iOS 16.0, *) {
                tableView.setNeedsLayout()
                tableView.layoutIfNeeded()
            } else {
                tableView.reloadData()
            }
        }

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
            runBlockOnEndDecelerating?()
            runBlockOnEndDecelerating = nil
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
                    tableView.setContentOffset(CGPointMake(0, tableView.contentOffset.y - 5), animated: false)
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
            if tableView.numberOfRows(inSection: 0) > index && index >= 0 {
                tableView.scrollToRow(
                    at: IndexPath(row: index, section: 0),
                    at: position,
                    animated: animated
                )
            } else {
                logger.error("Scroll: index out of bounds of table view")
            }
            setScrollEndedListener()
        }

        func setScrollEndedListener() {
            // wait until scrolling finishes so other calls will not interrupt it until then
            let task = Task {
                do {
                    try await Task.sleep(nanoseconds: 2000_000000)
                    // in case of listener will not be called
                    await MainActor.run {
                        representer.scrollState = .atDestination
                    }
                } catch {}
            }
            runBlockOnEndScrolling = {
                task.cancel()
                self.representer.scrollState = .atDestination
                self.runBlockOnEndScrolling = nil
            }
        }

        func update(_ items: [MergedItem]) {
            let prevSnapshot = dataSource.snapshot()
            let wasCount = prevSnapshot.numberOfItems
            let willBeCount = items.count
            let c = 1
            let insertedSeveralNewestItems = wasCount != 0 && willBeCount - wasCount == c && prevSnapshot.itemIdentifiers.first!.hashValue == items[c].hashValue
            logger.debug("LALAL WAS \(wasCount) will be \(items.count)")
            if items == prevSnapshot.itemIdentifiers {
                    logger.debug("LALAL SAME ITEMS, not rebuilding the tableview")
                    // update counters because they are static, unbound to specific chat and will become outdated if a new empty chat was open after non-empty one with unread messages
                    updateFloatingButtons.send()
                    return
                }
            var snapshot = NSDiffableDataSourceSnapshot<Section, MergedItem>()
            snapshot.appendSections([.main])
            snapshot.appendItems(items)

            dataSource.defaultRowAnimation = .none

            let wasContentHeight = tableView.contentSize.height
            let wasOffset = tableView.contentOffset.y
            let listState = getListState(items)
            let wasFirstVisibleRow = listState?.firstVisibleItemIndex ?? 0//tableView.indexPathsForVisibleRows?.first?.row ?? 0
            let wasFirstVisibleOffset = listState?.firstVisibleItemOffset ?? 0
            // Sets content offset on initial load
            if wasCount == 0 {
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
            } else if wasCount != snapshot.numberOfItems {
                logger.debug("LALAL drag \(self.tableView.isDragging), decel \(self.tableView.isDecelerating)")
                if useSmoothScrolling && tableView.isDecelerating {
                    tableView.panGestureRecognizer.isEnabled = false
                    tableView.beginUpdates()
                    dataSource.apply(
                        snapshot,
                        animatingDifferences: false
                    )
                    tableView.endUpdates()
                    tableView.panGestureRecognizer.isEnabled = true
                } else {
                    // remember current translation
                    var translationToApply: CGPoint? = nil
                    if let superview = self.tableView.superview {
                        let t = self.tableView.panGestureRecognizer.translation(in: superview)
                        if t.y != 0 {
                            translationToApply = t
                        }
                    }
                    dataSource.apply(
                        snapshot,
                        animatingDifferences: false
                    )
                    logger.debug("LALAL WAS LISTSTATE \(listState?.firstVisibleItemIndex ?? -3)  now \(self.getListState(items)?.firstVisibleItemIndex ?? -4)")
                    let newListState = getListState(items)
                    if listState?.firstVisibleItemIndex == newListState?.firstVisibleItemIndex {
                    // added new items to top
                    } else {
                        stopScrolling()
                    // added new items to bottom
                    //                        logger.debug("LALAL WAS HEIGHT \(wasContentHeight) now \(self.tableView.contentSize.height), offset was \(wasOffset), now \(self.tableView.contentOffset.y), will be \(self.tableView.contentOffset.y + (self.tableView.contentSize.height - wasContentHeight)), countDiff \(countDiff), wasVisibleRow \(wasFirstVisibleRow), wasFirstVisibleOffset \(wasFirstVisibleOffset)")
                        let countDiff = if let listState, let newListState {
                            newListState.firstVisibleItemIndex - listState.firstVisibleItemIndex
                        } else {
                            max(0, snapshot.numberOfItems - prevSnapshot.numberOfItems)
                        }
                    logger.debug("LALAL BEFORE SCROLLTOROW \(snapshot.numberOfItems - 1)  \(countDiff)  \(wasFirstVisibleRow)  \(self.tableView.contentOffset.y)  \(wasFirstVisibleOffset)")
                    getListState(items)
                    self.tableView.scrollToRow(
                        at: IndexPath(row: max(0, min(snapshot.numberOfItems - 1, countDiff + wasFirstVisibleRow)), section: 0),
                        at: .top,
                        animated: false
                    )
                    self.tableView.setContentOffset(
                        CGPoint(x: 0, y: self.tableView.contentOffset.y - wasFirstVisibleOffset),
                        animated: false
                    )
                    logger.debug("LALAL AFTER SCROLLTOROW")
                    let state = getListState(items)!
                    logger.debug("LALAL NOW FIRST VISIBLE \(state.firstVisibleItemIndex) \(state.firstVisibleItemOffset)")

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
                }
            }
            updateFloatingButtons.send()
        }

        override func scrollViewDidScroll(_ scrollView: UIScrollView) {
            updateFloatingButtons.send()

            if let listState = self.getListState(representer.mergedItems.items) {
                if runBlockOnEndDecelerating != nil && nearSplit(listState, representer.mergedItems) {
                    stopScrolling()
                }
                preloadIfNeeded(listState)
            }
        }

        override public func scrollViewDidEndScrollingAnimation(_ scrollView: UIScrollView) {
            if let onEnd = runBlockOnEndScrolling {
                DispatchQueue.main.async {
                    onEnd()
                }
            }
        }

        func stopScrolling() {
            tableView.setContentOffset(self.tableView.contentOffset, animated: false)
        }

        func nearSplit(_ listState: ListState, _ mergedItems: MergedItems) -> Bool {
            if mergedItems.splits.isEmpty { return false }

            let remaining = 10
            let firstVisibleIndex = listState.firstVisibleItemIndex
            let lastVisibleIndex = listState.lastVisibleItemIndex
            for split in mergedItems.splits {
                // before any split
                if split.indexRangeInParentItems.lowerBound > firstVisibleIndex {
                    if lastVisibleIndex > (split.indexRangeInParentItems.lowerBound - remaining) {
                        return true
                    }
                    break
                }
                let containsInRange = split.indexRangeInParentItems.contains(firstVisibleIndex)
                if containsInRange {
                    if lastVisibleIndex > (split.indexRangeInParentItems.upperBound - remaining) {
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

        func getListState(_ items: [MergedItem]) -> ListState? {
            logger.debug("LALAL VISIBLE ROWS \((self.tableView.indexPathsForVisibleRows ?? []).map({ $0.row }))")
            if let visibleRows = tableView.indexPathsForVisibleRows,
               visibleRows.last?.row ?? 0 < items.count {
                let scrollOffset: Double = tableView.contentOffset.y + InvertedTableView.inset
                let topItemDate: Date? =
                if let lastVisible = visibleRows.last(where: { isVisible(indexPath: $0) }) {
                    items[lastVisible.item].oldest().item.meta.itemTs
                } else {
                    nil
                }
                let firstVisible = visibleRows.first(where: { isVisible(indexPath: $0) })
                let firstVisibleOffset: CGFloat? = if let row = firstVisible?.row {
                    offsetForRow(row)
                } else { nil }
                let lastVisible = visibleRows.last(where: { isVisible(indexPath: $0) })
                logger.debug("LALAL LAST \(lastVisible?.item ?? -2)")
                let bottomItemId: ChatItem.ID? =
                if let firstVisible {
                    items[firstVisible.row].newest().item.id
                } else {
                    nil
                }
                return ListState(scrollOffset: scrollOffset, topItemDate: topItemDate, bottomItemId: bottomItemId, firstVisibleItemIndex: firstVisible?.row ?? 0, lastVisibleItemIndex: lastVisible?.row ?? 0, firstVisibleItemOffset: firstVisibleOffset ?? 0)
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

        func scrollToItem(_ itemId: Int64) {
            if scrollToItemInProgress {
                return
            }
            scrollToItemInProgress = true
            Task {
                logger.debug("LALAL SCROLL TO ITEM \(itemId)  \(ItemsModel.shared.reversedChatItems.count)")
                do {
                    var index = representer.mergedItems.indexInParentItems[itemId]
                    // setting it to 'loading' even if the item is loaded because in rare cases when the resulting item is near the top, scrolling to
                    // it will trigger loading more items and will scroll to incorrect position (because of trimming)
                    await MainActor.run {
                        representer.loadingMoreItems = true
                    }
                    logger.debug("LALAL SCROLL TO ITEM \(index ?? -2)")
                    if index == nil {
                        let pagination = ChatPagination.around(chatItemId: itemId, count: ChatPagination.PRELOAD_COUNT * 2)
                        let oldSize = ItemsModel.shared.reversedChatItems.count
                        _ = await self.representer.loadItems(true, pagination, { self.visibleItemIndexesNonReversed(Binding.constant(self.representer.mergedItems))})
                        var repeatsLeft = 50
                        while oldSize == ItemsModel.shared.reversedChatItems.count && repeatsLeft > 0 {
                            try await Task.sleep(nanoseconds: 20_000000)
                            repeatsLeft -= 1
                            logger.debug("LALAL SCROLL REPEATS \(repeatsLeft)")
                        }
                        index = representer.mergedItems.indexInParentItems[itemId]
                    }
                    logger.debug("LALAL SCROLL TO ITEM2 \(index ?? -3) \(ItemsModel.shared.reversedChatItems.count)")
                    if let index {
                        scroll(to: min(ItemsModel.shared.reversedChatItems.count - 1, index), position: .bottom)
                    } else {
                        await MainActor.run {
                            representer.scrollState = .atDestination
                        }
                    }
                } catch {
                    logger.error("Error scrolling to item: \(error)")
                }
                await MainActor.run {
                    representer.loadingMoreItems = false
                    scrollToItemInProgress = false
                }
            }
        }

        func preloadIfNeeded(_ state: ListState) {
            if representer.scrollState == .atDestination, prevFirstVisible != state.firstVisibleItemIndex {
                logger.debug("LALAL LOADING before \(String(describing: self.representer.scrollState))")
                prevFirstVisible = state.firstVisibleItemIndex
                Task {
                    await preloadItems(self.representer.mergedItems, self.representer.allowLoadMoreItems, state, self.representer.$ignoreLoadingRequests) { pagination in
                        logger.debug("LALAL LOADING INSIDE")
                        let triedToLoad = await self.representer.loadItems(false, pagination, { self.visibleItemIndexesNonReversed(Binding.constant(self.representer.mergedItems)) })
                        return triedToLoad
                    }
                }
            }
        }
    }

    /// `UIHostingConfiguration` back-port for iOS14 and iOS15
    /// Implemented as a `UITableViewCell` that wraps and manages a generic `UIHostingController`
    private final class HostingCell<Hosted: View>: UITableViewCell {
        private let hostingController = UIHostingController<Hosted?>(rootView: nil)

        /// Updates content of the cell
        /// For reference: https://noahgilmore.com/blog/swiftui-self-sizing-cells/
        func set(content: Hosted, parent: UIViewController) {
            hostingController.view.backgroundColor = .clear
            hostingController.rootView = content
            if let hostingView = hostingController.view {
                hostingView.invalidateIntrinsicContentSize()
                if hostingController.parent != parent { parent.addChild(hostingController) }
                if !contentView.subviews.contains(hostingController.view) {
                    contentView.addSubview(hostingController.view)
                    hostingView.translatesAutoresizingMaskIntoConstraints = false
                    NSLayoutConstraint.activate([
                        hostingView.leadingAnchor
                            .constraint(equalTo: contentView.leadingAnchor),
                        hostingView.trailingAnchor
                            .constraint(equalTo: contentView.trailingAnchor),
                        hostingView.topAnchor
                            .constraint(equalTo: contentView.topAnchor),
                        hostingView.bottomAnchor
                            .constraint(equalTo: contentView.bottomAnchor)
                    ])
                }
                if hostingController.parent != parent { hostingController.didMove(toParent: parent) }
            } else {
                fatalError("Hosting View not loaded \(hostingController)")
            }
        }

        override func prepareForReuse() {
            super.prepareForReuse()
            hostingController.rootView = nil
        }
    }
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

/// Manages ``ReverseList`` scrolling
class ReverseListScrollModel: ObservableObject {
    /// Represents Scroll State of ``ReverseList``
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
        state = .scrollingTo(.bottom)
    }

    func scrollToItem(id: ChatItem.ID) {
        state = .scrollingTo(.item(id))
    }

    func scrollToRow(row: Int) {
        state = .scrollingTo(.row(row))
    }
}

fileprivate let cellReuseId = "hostingCell"

fileprivate let notificationName = NSNotification.Name(rawValue: "reverseListNeedsLayout")

fileprivate extension CGAffineTransform {
    /// Transform that vertically flips the view, preserving it's location
    static let verticalFlip = CGAffineTransform(scaleX: 1, y: -1)
}

extension NotificationCenter {
    static func postReverseListNeedsLayout() {
        NotificationCenter.default.post(
            name: notificationName,
            object: nil
        )
    }
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

//    override var contentOffset: CGPoint {
//        get { super.contentOffset }
//        set {
//            logger.debug("LALAL SET OFFSET \(newValue.y)")
//            if newValue.y <= 0 {
//                return
//            }
//            super.contentOffset = newValue
//        }
//    }

}
