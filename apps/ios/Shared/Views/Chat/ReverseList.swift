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
    let items: Array<ChatItem>
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
        if case let .scrollingTo(destination) = scrollState, !items.isEmpty {
            controller.view.layer.removeAllAnimations()
            switch destination {
            case .nextPage:
                controller.scrollToNextPage()
            case let .item(id):
                let row = items.firstIndex(where: { $0.id == id })
                if let row {
                    controller.scroll(to: row, position: .bottom)
                } else {
                    controller.scrollToItem(id)
                }
            case let .row(row):
                controller.scroll(to: row, position: .bottom)
            case .bottom:
                controller.scroll(to: 0, position: .top)
            }
        } else {
            // when tableView is dragging and new items are added, scroll position cannot be set correctly
            // so it's better to just wait until dragging ends
            if controller.tableView.isDragging/* && !controller.tableView.isDecelerating*/ {
                controller.runBlockOnEndDecelerating = {
                    controller.update(items: items)
                }
            } else {
                controller.runBlockOnEndDecelerating = nil
                controller.update(items: items)
            }
        }
    }

    /// Controller, which hosts SwiftUI cells
    public class Controller: UITableViewController {
        private enum Section { case main }
        var representer: ReverseList
        // Here Int means hash of the ChatItem that is inside MergedItem.newest().item.hashValue.
        // Putting MergedItem here directly prevents UITableViewDiffableDataSource to make partial updates
        // which looks like UITableView scrolls to bottom on insert values to bottom instead of
        // remains in the same scroll position
        private var dataSource: UITableViewDiffableDataSource<Section, Int>!
        var itemCount: Int {
            get {
                representer.mergedItems.items.count
            }
        }
        private var itemsInPrevSnapshot: Dictionary<Int, MergedItem> = [:]
        private let updateFloatingButtons = PassthroughSubject<Void, Never>()
        private var bag = Set<AnyCancellable>()

        var runBlockOnEndDecelerating: (() -> Void)? = nil

        private var scrollToRowOnAppear = 0
        private var translationToApply: CGPoint? = nil

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

            var prevFirstVisible = -1

            // 3. Configure data source
            self.dataSource = UITableViewDiffableDataSource<Section, Int>(
                tableView: tableView
            ) { (tableView, indexPath, item) -> UITableViewCell? in
                //if indexPath.item > self.itemCount - 8 {
                logger.debug("LALAL ITEM \(indexPath.item)")
                Task {
                    // do not put it outside Task because it will be stackoverflow error
                    let state = await MainActor.run {
                        self.getListState()
                    }
                    if let state, prevFirstVisible != state.firstVisibleItemIndex {
                        logger.debug("LALAL LOADING before")
                        if representer.scrollState != .atDestination {
                            // do not try to load anything in order to not interupt scrolling
                            return
                        }
                        prevFirstVisible = state.firstVisibleItemIndex
                        await preloadItems(self.representer.mergedItems, self.representer.allowLoadMoreItems, state, self.representer.$ignoreLoadingRequests) { pagination in
                            logger.debug("LALAL LOADING INSIDE")
                            let triedToLoad = await self.representer.loadItems(false, pagination, { self.visibleItemIndexesNonReversed(Binding.constant(self.representer.mergedItems)) })
                            let superview = self.tableView.superview
                            if triedToLoad, let superview {
                                let t = self.tableView.panGestureRecognizer.translation(in: superview)
                                if t.y != 0 {
                                    self.translationToApply = t
                                }
                                // stop scrolling
                                self.tableView.setContentOffset(self.tableView.contentOffset, animated: false)
                            }
                            return triedToLoad
                        }
                    }
                }
                //}
                let cell = tableView.dequeueReusableCell(withIdentifier: cellReuseId, for: indexPath)
                if #available(iOS 16.0, *) {
                    cell.contentConfiguration = UIHostingConfiguration { self.representer.content(indexPath.item, self.itemsInPrevSnapshot[item]!) }
                        .margins(.all, 0)
                        .minSize(height: 1) // Passing zero will result in system default of 44 points being used
                } else {
                    if let cell = cell as? HostingCell<Content> {
                        cell.set(content: self.representer.content(indexPath.item, self.itemsInPrevSnapshot[item]!), parent: self)
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
                    if let listState = DispatchQueue.main.sync(execute: { [weak self] in self?.getListState() }) {
                        ChatView.FloatingButtonModel.shared.updateOnListChange(representer.mergedItems, listState)
                    }
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

        /// Scrolls up
        func scrollToNextPage() {
            tableView.setContentOffset(
                CGPoint(
                    x: tableView.contentOffset.x,
                    y: tableView.contentOffset.y + tableView.bounds.height
                ),
                animated: true
            )
            Task { representer.scrollState = .atDestination }
        }

        /// Scrolls to Item at index path
        /// - Parameter indexPath: Item to scroll to - will scroll to beginning of the list, if `nil`
        func scroll(to index: Int?, position: UITableView.ScrollPosition) {
            var animated = false
            if #available(iOS 16.0, *) {
                animated = true
            }
            if let index, tableView.numberOfRows(inSection: 0) > index {
                tableView.scrollToRow(
                    at: IndexPath(row: index, section: 0),
                    at: position,
                    animated: animated
                )
            } else {
                tableView.setContentOffset(
                    CGPoint(x: .zero, y: -InvertedTableView.inset),
                    animated: animated
                )
            }
            Task { representer.scrollState = .atDestination }
        }

        func update(items: [ChatItem]) {
            var prevSnapshot = dataSource.snapshot()
            let wasCount = prevSnapshot.numberOfItems
            let willBeCount = representer.mergedItems.items.count
            let c = 13
            let insertedOneNewestItem = wasCount != 0 && willBeCount - wasCount == c && prevSnapshot.itemIdentifiers.first!.hashValue == self.representer.mergedItems.items[c].hashValue
            logger.debug("LALAL WAS \(wasCount) will be \(self.representer.mergedItems.items.count)")
            //self.representer.mergedItems = MergedItems.create(items, representer.unreadCount, representer.revealedItems, ItemsModel.shared.chatState)
            let snapshot: NSDiffableDataSourceSnapshot<Section, Int>
            let itemsInCurrentSnapshot: Dictionary<Int, MergedItem>
            if insertedOneNewestItem {
                for i in 0 ..< c {
                    prevSnapshot.insertItems([representer.mergedItems.items[c - i].hashValue], beforeItem: prevSnapshot.itemIdentifiers.first!)
                }
                var new = itemsInPrevSnapshot
                new[representer.mergedItems.items.first!.hashValue] = representer.mergedItems.items.first!
                itemsInCurrentSnapshot = new
                snapshot = prevSnapshot
            } else {
                var new: Dictionary<Int, MergedItem> = [:]
                var snap = NSDiffableDataSourceSnapshot<Section, Int>()
                snap.appendSections([.main])
                snap.appendItems(representer.mergedItems.items.map({ merged in
                    new[merged.hashValue] = merged
                    return merged.hashValue
                }))
                itemsInCurrentSnapshot = new

                if snap.itemIdentifiers == prevSnapshot.itemIdentifiers {
                    logger.debug("LALAL SAME ITEMS, not rebuilding the tableview")
                    return
                }

                snapshot = snap
            }
            dataSource.defaultRowAnimation = .none
            let wasContentHeight = tableView.contentSize.height
            let wasOffset = tableView.contentOffset.y
            let listState = getListState()
            let wasFirstVisibleRow = listState?.firstVisibleItemIndex ?? 0//tableView.indexPathsForVisibleRows?.first?.row ?? 0
            let wasFirstVisibleOffset = listState?.firstVisibleItemOffset ?? 0
            let countDiff = max(0, snapshot.numberOfItems - prevSnapshot.numberOfItems)
            // Sets content offset on initial load
            if wasCount == 0 {
                itemsInPrevSnapshot = itemsInCurrentSnapshot
                dataSource.apply(
                    snapshot,
                    animatingDifferences: insertedOneNewestItem
                )
                if let firstUnreadItem = snapshot.itemIdentifiers.lastIndex(where: { hash in itemsInPrevSnapshot[hash]!.hasUnread() }) {
                    scrollToRowWhenKnowSize(firstUnreadItem)
                } else {
                    tableView.setContentOffset(
                        CGPoint(x: 0, y: -InvertedTableView.inset),
                        animated: false
                    )
                }
            } else if wasCount != snapshot.numberOfItems {
                logger.debug("LALAL drag \(self.tableView.isDragging), decel \(self.tableView.isDecelerating)")
                //                tableView.panGestureRecognizer.isEnabled = false
                //                logger.debug("LALAL drag2 \(self.tableView.isDragging), decel \(self.tableView.isDecelerating)")
                if tableView.isDecelerating {
                    //                    CATransaction.begin()
                    itemsInPrevSnapshot = itemsInCurrentSnapshot
                    tableView.beginUpdates()
                    dataSource.apply(
                        snapshot,
                        animatingDifferences: false
                    )
                    tableView.endUpdates()
                    //                    CATransaction.commit()
                    tableView.panGestureRecognizer.isEnabled = true
                } else {
                    itemsInPrevSnapshot = itemsInCurrentSnapshot
                    dataSource.apply(
                        snapshot,
                        animatingDifferences: false
                    )
                    //if snapshot.itemIdentifiers[0] == prevSnapshot.itemIdentifiers[0] {
                    // added new items to top

                    //} else {
                    // added new items to bottom
                    //                        logger.debug("LALAL WAS HEIGHT \(wasContentHeight) now \(self.tableView.contentSize.height), offset was \(wasOffset), now \(self.tableView.contentOffset.y), will be \(self.tableView.contentOffset.y + (self.tableView.contentSize.height - wasContentHeight)), countDiff \(countDiff), wasVisibleRow \(wasFirstVisibleRow), wasFirstVisibleOffset \(wasFirstVisibleOffset)")
                    self.tableView.scrollToRow(
                        at: IndexPath(row: max(0, min(snapshot.numberOfItems - 1, countDiff + wasFirstVisibleRow)), section: 0),
                        at: .top,
                        animated: false
                    )
                    self.tableView.setContentOffset(
                        CGPoint(x: 0, y: self.tableView.contentOffset.y - wasFirstVisibleOffset),
                        animated: false
                    )
                    let state = getListState()!
                    logger.debug("LALAL NOW FIRST VISIBLE \(state.firstVisibleItemIndex) \(state.firstVisibleItemOffset)")

                    if let t = translationToApply {
                        //                        self.tableView.panGestureRecognizer.setTranslation(CGPointMake(t.x, t.y), in: self.tableView.superview!)
                        let o = self.tableView.contentOffset
                        //                        UIView.animate(withDuration: 0.5) {
                        //                            self.tableView.setContentOffset(CGPointMake(o.x + t.x, o.y + t.y), animated: false)
                        //                            self.tableView.layoutIfNeeded()
                        //                        }
                        self.tableView.setContentOffset(CGPointMake(o.x + t.x, o.y + t.y), animated: true)
                        translationToApply = nil
                    }
                    //}
                }
            }
            updateFloatingButtons.send()
            return
        }

        override func scrollViewDidScroll(_ scrollView: UIScrollView) {
            updateFloatingButtons.send()
        }

        func getListState() -> ListState? {
            logger.debug("LALAL VISIBLE ROWS \((self.tableView.indexPathsForVisibleRows ?? []).map({ $0.row }))")
            if let visibleRows = tableView.indexPathsForVisibleRows,
               visibleRows.last?.row ?? 0 < representer.mergedItems.items.count {
                let scrollOffset: Double = tableView.contentOffset.y + InvertedTableView.inset
                let topItemDate: Date? =
                if let lastVisible = visibleRows.last(where: { isVisible(indexPath: $0) }) {
                    representer.mergedItems.items[lastVisible.item].oldest().item.meta.itemTs
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
                    representer.mergedItems.items[firstVisible.row].newest().item.id
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
            case nextPage
            case item(ChatItem.ID)
            case row(Int)
            case bottom
        }

        case scrollingTo(Destination)
        case atDestination
    }

    @Published var state: State = .atDestination

    func scrollToNextPage() {
        state = .scrollingTo(.nextPage)
    }

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
