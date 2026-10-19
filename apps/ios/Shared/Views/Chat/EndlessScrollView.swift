//
//  EndlessScrollView.swift
//  SimpleX (iOS)
//
//  Created by Stanislav Dmitrenko on 25.01.2025.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ScrollRepresentable<Content: View, ScrollItem>: UIViewControllerRepresentable where ScrollItem : Identifiable, ScrollItem: Hashable {

    let scrollView: EndlessScrollView<ScrollItem>
    let content: (Int, ScrollItem) -> Content

    func makeUIViewController(context: Context) -> ScrollController {
        ScrollController.init(scrollView: scrollView, content: content)
    }

    func updateUIViewController(_ controller: ScrollController, context: Context) {}

    class ScrollController: UIViewController {
        let scrollView: EndlessScrollView<ScrollItem>
        fileprivate var items: [ScrollItem] = []
        fileprivate var content: ((Int, ScrollItem) -> Content)!

        fileprivate init(scrollView: EndlessScrollView<ScrollItem>, content: @escaping (Int, ScrollItem) -> Content) {
            self.scrollView = scrollView
            self.content = content
            super.init(nibName: nil, bundle: nil)
            self.view = scrollView
            scrollView.createCell = createCell
            scrollView.updateCell = updateCell
        }

        required init?(coder: NSCoder) { fatalError() }

        private func createCell(_ index: Int, _ items: [ScrollItem], _ cellsToReuse: inout [UIView]) -> UIView {
            let item: ScrollItem? = index >= 0 && index < items.count ? items[index] : nil
            let cell: UIView
            if #available(iOS 16.0, *), false {
                let c: UITableViewCell = cellsToReuse.isEmpty ? UITableViewCell() : cellsToReuse.removeLast() as! UITableViewCell
                if let item {
                    c.contentConfiguration = UIHostingConfiguration { self.content(index, item) }
                        .margins(.all, 0)
                        .minSize(height: 1) // Passing zero will result in system default of 44 points being used
                }
                cell = c
            } else {
                let c = cellsToReuse.isEmpty ? HostingCell<Content>() : cellsToReuse.removeLast() as! HostingCell<Content>
                if let item {
                    c.set(content: self.content(index, item), parent: self)
                }
                cell = c
            }
            cell.isHidden = false
            cell.backgroundColor = .clear
            let size = cell.systemLayoutSizeFitting(CGSizeMake(scrollView.bounds.width, CGFloat.greatestFiniteMagnitude))
            cell.frame.size.width = scrollView.bounds.width
            cell.frame.size.height = size.height
            return cell
        }

        private func updateCell(cell: UIView, _ index: Int, _ items: [ScrollItem]) {
            let item = items[index]
            if #available(iOS 16.0, *), false {
                (cell as! UITableViewCell).contentConfiguration = UIHostingConfiguration { self.content(index, item) }
                    .margins(.all, 0)
                    .minSize(height: 1) // Passing zero will result in system default of 44 points being used
            } else {
                if let cell = cell as? HostingCell<Content> {
                    cell.set(content: self.content(index, item), parent: self)
                } else {
                    fatalError("Unexpected Cell Type for: \(item)")
                }
            }
            let size = cell.systemLayoutSizeFitting(CGSizeMake(scrollView.bounds.width, CGFloat.greatestFiniteMagnitude))
            cell.frame.size.width = scrollView.bounds.width
            cell.frame.size.height = size.height
            cell.setNeedsLayout()
        }
    }
}

class EndlessScrollView<ScrollItem>: UIScrollView, UIScrollViewDelegate, UIGestureRecognizerDelegate where ScrollItem : Identifiable, ScrollItem: Hashable {

    /// Stores actual state of the scroll view and all elements drawn on the screen
    let listState: ListState = ListState()

    /// Just some random big number that will probably be enough to scrolling down and up without reaching the end
    var initialOffset: CGFloat = 100000000

    /// Default item id when no items in the visible items list. Something that will never be in real data
    fileprivate static var DEFAULT_ITEM_ID: any Hashable { get { Int64.min } }

    /// Storing an offset that was already used for laying down content to be able to see the difference
    var prevProcessedOffset: CGFloat = 0

    /// When screen is being rotated, it's important to track the view size and adjust scroll offset accordingly because the view doesn't know that the content
    /// starts from bottom and ends at top, not vice versa as usual
    var oldScreenHeight: CGFloat = 0

    /// Not 100% correct height of the content since the items loaded lazily and their dimensions are unkown until they are on screen
    var estimatedContentHeight: ContentHeight = ContentHeight()

    /// Specify here the value that is small enough to NOT see any weird animation when you scroll to items. Minimum expected item size is ok. Scroll speed depends on it too
    var averageItemHeight: CGFloat = 30

    /// This is used as a multiplier for difference between current index and scrollTo index using [averageItemHeight] as well. Increase it to get faster speed
    var scrollStepMultiplier: CGFloat = 0.37

    /// Adds content padding to top
    var insetTop: CGFloat = 100

    /// Adds content padding to bottom
    var insetBottom: CGFloat = 100

    var scrollToItemIndexDelayed: Int? = nil

    /// The second scroll view that is used only for purpose of displaying scroll bar with made-up content size and scroll offset that is gathered from main scroll view, see [estimatedContentHeight]
    let scrollBarView: UIScrollView = UIScrollView(frame: .zero)

    /// Stores views that can be used to hold new content so it will be faster to replace something than to create the whole view from scratch
    var cellsToReuse: [UIView] = []

    /// Enable debug to see hundreds of logs
    var debug: Bool = false

    var createCell: (Int, [ScrollItem], inout [UIView]) -> UIView? = { _, _, _ in nil }
    var updateCell: (UIView, Int, [ScrollItem]) -> Void = { cell, _, _ in }


    override init(frame: CGRect) {
        super.init(frame: frame)
        self.delegate = self
    }

    required init?(coder: NSCoder) { fatalError() }

    class ListState: NSObject {

        /// Will be called on every change of the items array, visible items, and scroll position
        var onUpdateListener: () -> Void = {}

        /// Items that were used to lay out the screen
        var items: [ScrollItem] = [] {
            didSet {
                onUpdateListener()
            }
        }

        /// It is equai to the number of [items]
        var totalItemsCount: Int {
            items.count
        }

        /// The items with their positions and other useful information. Only those that are visible on screen
        var visibleItems: [EndlessScrollView<ScrollItem>.VisibleItem] = []

        /// Index in [items] of the first item on screen. This is intentiallty not derived from visible items because it's is used as a starting point for laying out the screen
        var firstVisibleItemIndex: Int = 0

        /// Unique item id of the first visible item on screen
        var firstVisibleItemId: any Hashable = EndlessScrollView<ScrollItem>.DEFAULT_ITEM_ID

        /// Item offset of the first item on screen. Most of the time it's non-positive but it can be positive as well when a user produce overscroll effect on top/bottom of the scroll view
        var firstVisibleItemOffset: CGFloat = -100

        /// Index of the last visible item on screen
        var lastVisibleItemIndex: Int {
            visibleItems.last?.index ?? 0
        }

        /// Specifies if visible items cover the whole screen or can cover it (if overscrolled)
        var itemsCanCoverScreen: Bool = false

        /// Whether there is a non-animated scroll to item in progress or not
        var isScrolling: Bool = false
        /// Whether there is an animated scroll to item in progress or not
        var isAnimatedScrolling: Bool = false

        override init() {
            super.init()
        }
    }

    class VisibleItem {
        let index: Int
        let item: ScrollItem
        let view: UIView
        var offset: CGFloat

        init(index: Int, item: ScrollItem, view: UIView, offset: CGFloat) {
            self.index = index
            self.item = item
            self.view = view
            self.offset = offset
        }
    }

    class ContentHeight {
        /// After that you should see overscroll effect. When scroll positon is far from
        /// top/bottom items, these values are estimated based on items count multiplied by averageItemHeight or real item height (from visible items). Example:
        /// [ 10, 9, 8, 7, (6, 5, 4, 3), 2, 1, 0] - 6, 5, 4, 3 are visible and have know heights but others have unknown height and for them averageItemHeight will be used to calculate the whole content height
        var topOffsetY: CGFloat = 0
        var bottomOffsetY: CGFloat = 0

        var virtualScrollOffsetY: CGFloat = 0

        /// How much distance were overscolled on top which often means to show sticky scrolling that should scroll back to real position after a users finishes dragging the scrollView
        var overscrolledTop: CGFloat = 0

        /// Adds content padding to bottom and top
        var inset: CGFloat = 100

        /// Estimated height of the contents of scroll view
        var height: CGFloat {
            get { bottomOffsetY - topOffsetY }
        }

        /// Estimated height of the contents of scroll view + distance of overscrolled effect. It's only updated when number of item changes to prevent jumping of scroll bar
        var virtualOverscrolledHeight: CGFloat {
            get {
                bottomOffsetY - topOffsetY + overscrolledTop - inset * 2
            }
        }

        func update(
            _ contentOffset: CGPoint,
            _ listState: ListState,
            _ averageItemHeight: CGFloat,
            _ updateStaleHeight: Bool
        ) {
            let lastVisible = listState.visibleItems.last
            let firstVisible = listState.visibleItems.first
            guard let last = lastVisible, let first = firstVisible else {
                topOffsetY = contentOffset.y
                bottomOffsetY = contentOffset.y
                virtualScrollOffsetY = 0
                overscrolledTop = 0
                return
            }
            topOffsetY = last.view.frame.origin.y - CGFloat(listState.totalItemsCount - last.index - 1) * averageItemHeight - self.inset
            bottomOffsetY = first.view.frame.origin.y + first.view.bounds.height + CGFloat(first.index) * averageItemHeight + self.inset
            virtualScrollOffsetY = contentOffset.y - topOffsetY
            overscrolledTop = max(0, last.index == listState.totalItemsCount - 1 ? last.view.frame.origin.y - contentOffset.y : 0)
        }
    }

    var topY: CGFloat {
        get { contentOffset.y }
    }

    var bottomY: CGFloat {
        get { contentOffset.y + bounds.height }
    }

    override func layoutSubviews() {
        super.layoutSubviews()
        if contentSize.height == 0 {
            setup()
        }
        let newScreenHeight = bounds.height
        if newScreenHeight != oldScreenHeight && oldScreenHeight != 0 {
            contentOffset.y += oldScreenHeight - newScreenHeight
            scrollBarView.frame = CGRectMake(frame.width - 10, self.insetTop, 10, frame.height - self.insetTop - self.insetBottom)
        }
        oldScreenHeight = newScreenHeight
        adaptItems(listState.items, false)
        if let index = scrollToItemIndexDelayed {
            scrollToItem(index)
            scrollToItemIndexDelayed = nil
        }
    }

    private func setup() {
        contentSize = CGSizeMake(frame.size.width, initialOffset * 2)
        prevProcessedOffset = initialOffset
        contentOffset = CGPointMake(0, initialOffset)
        
        showsVerticalScrollIndicator = false
        scrollBarView.showsHorizontalScrollIndicator = false
        panGestureRecognizer.delegate = self
        addGestureRecognizer(scrollBarView.panGestureRecognizer)
        superview!.addSubview(scrollBarView)
    }

    func updateItems(_ items: [ScrollItem], _ forceReloadVisible: Bool = false) {
        if !Thread.isMainThread {
            logger.error("Use main thread to update items")
            return
        }
        if bounds.height == 0 {
            self.listState.items = items
            // this function requires to have valid bounds and it will be called again once it has them
            return
        }
        adaptItems(items, forceReloadVisible)
        snapToContent(animated: false)
    }

    /// [forceReloadVisible]: reloads every item that was visible regardless of hashValue changes
    private func adaptItems(_ items: [ScrollItem], _ forceReloadVisible: Bool, overridenOffset: CGFloat? = nil) {
        let start = Date.now
        // special case when everything was removed
        if items.isEmpty {
            listState.visibleItems.forEach { item in item.view.removeFromSuperview() }
            listState.visibleItems = []
            listState.itemsCanCoverScreen = false
            listState.firstVisibleItemId = EndlessScrollView<ScrollItem>.DEFAULT_ITEM_ID
            listState.firstVisibleItemIndex = 0
            listState.firstVisibleItemOffset = -insetTop

            estimatedContentHeight.update(contentOffset, listState, averageItemHeight, true)
            scrollBarView.contentSize = .zero
            scrollBarView.contentOffset = .zero

            prevProcessedOffset = contentOffset.y
            // this check is just to prevent didSet listener from firing on the same empty array, no use for this
            if !self.listState.items.isEmpty {
                self.listState.items = items
            }
            return
        }

        let contentOffsetY = overridenOffset ?? contentOffset.y

        var oldVisible = listState.visibleItems
        var newVisible: [VisibleItem] = []
        var visibleItemsHeight: CGFloat = 0
        let offsetsDiff = contentOffsetY - prevProcessedOffset

        var shouldBeFirstVisible = items.firstIndex(where: { item in item.id == listState.firstVisibleItemId as! ScrollItem.ID }) ?? 0

        var wasFirstVisibleItemOffset = listState.firstVisibleItemOffset
        var alreadyChangedIndexWhileScrolling = false
        var allowOneMore = false
        var nextOffsetY: CGFloat = 0
        var i = shouldBeFirstVisible
        // building list of visible items starting from the first one that should be visible
        while i >= 0 && i < items.count {
            let item = items[i]
            let visibleIndex = oldVisible.firstIndex(where: { vis in vis.item.id == item.id })
            let visible: VisibleItem?
            if let visibleIndex {
                let v = oldVisible.remove(at: visibleIndex)
                if forceReloadVisible || v.view.bounds.width != bounds.width || v.item.hashValue != item.hashValue {
                    let wasHeight = v.view.bounds.height
                    updateCell(v.view, i, items)
                    if wasHeight < v.view.bounds.height && i == 0 && shouldBeFirstVisible == i {
                        v.view.frame.origin.y -= v.view.bounds.height - wasHeight
                    }
                }
                visible = v
            } else {
                visible = nil
            }
            if shouldBeFirstVisible == i {
                if let vis = visible {

                    if // there is auto scroll in progress and the first item has a higher offset than bottom part
                        // of the screen. In order to make scrolling down & up equal in time, we treat this as a sign to
                        // re-make the first visible item
                        (listState.isAnimatedScrolling && vis.view.frame.origin.y + vis.view.bounds.height < contentOffsetY + bounds.height) ||
                        // the fist visible item previously is hidden now, remove it and move on
                        !isVisible(vis.view) {
                        let newIndex: Int
                        if listState.isAnimatedScrolling {
                            // skip many items to make the scrolling take less time
                            var indexDiff = !alreadyChangedIndexWhileScrolling ? Int(ceil(abs(offsetsDiff / averageItemHeight))) : 0
                            // if index was already changed, no need to change it again. Otherwise, the scroll will overscoll and return back animated. Because it means the whole screen was scrolled
                            alreadyChangedIndexWhileScrolling = true

                            indexDiff = offsetsDiff <= 0 ? indexDiff : -indexDiff
                            newIndex = max(0, min(items.count - 1, i + indexDiff))
                            // offset for the first visible item can now be 0 because the previous first visible item doesn't exist anymore
                            wasFirstVisibleItemOffset = 0
                        } else {
                            // don't skip multiple items if it's manual scrolling gesture
                            newIndex = i + (offsetsDiff <= 0 ? 1 : -1)
                        }
                        shouldBeFirstVisible = newIndex
                        i = newIndex

                        cellsToReuse.append(vis.view)
                        hideAndRemoveFromSuperviewIfNeeded(vis.view)
                        continue
                    }
                }
                let vis: VisibleItem
                if let visible {
                    vis = VisibleItem(index: i, item: item, view: visible.view, offset: offsetToBottom(visible.view))
                } else {
                    let cell = createCell(i, items, &cellsToReuse)!
                    cell.frame.origin.y = bottomY + wasFirstVisibleItemOffset - cell.frame.height
                    vis = VisibleItem(index: i, item: item, view: cell, offset: offsetToBottom(cell))
                }
                if vis.view.superview == nil {
                    addSubview(vis.view)
                }
                newVisible.append(vis)
                visibleItemsHeight += vis.view.frame.height
                nextOffsetY = vis.view.frame.origin.y
            } else {
                let vis: VisibleItem
                if let visible {
                    vis = VisibleItem(index: i, item: item, view: visible.view, offset: offsetToBottom(visible.view))
                    nextOffsetY -= vis.view.frame.height
                    vis.view.frame.origin.y = nextOffsetY
                } else {
                    let cell = createCell(i, items, &cellsToReuse)!
                    nextOffsetY -= cell.frame.height
                    cell.frame.origin.y = nextOffsetY
                    vis = VisibleItem(index: i, item: item, view: cell, offset: offsetToBottom(cell))
                }
                if vis.view.superview == nil {
                    addSubview(vis.view)
                }
                newVisible.append(vis)
                visibleItemsHeight += vis.view.frame.height
            }
            if abs(nextOffsetY) < contentOffsetY && !allowOneMore {
                break
            } else if abs(nextOffsetY) < contentOffsetY {
                allowOneMore = false
            }
            i += 1
        }
        if let firstVisible = newVisible.first, firstVisible.view.frame.origin.y + firstVisible.view.frame.height < contentOffsetY + bounds.height, firstVisible.index > 0 {
            var offset: CGFloat = firstVisible.view.frame.origin.y + firstVisible.view.frame.height
            let index = firstVisible.index
            for i in stride(from: index - 1, through: 0, by: -1) {
                let item = items[i]
                let visibleIndex = oldVisible.firstIndex(where: { vis in vis.item.id == item.id })
                let vis: VisibleItem
                if let visibleIndex {
                    let visible = oldVisible.remove(at: visibleIndex)
                    visible.view.frame.origin.y = offset
                    vis = VisibleItem(index: i, item: item, view: visible.view, offset: offsetToBottom(visible.view))
                } else {
                    let cell = createCell(i, items, &cellsToReuse)!
                    cell.frame.origin.y = offset
                    vis = VisibleItem(index: i, item: item, view: cell, offset: offsetToBottom(cell))
                }
                if vis.view.superview == nil {
                    addSubview(vis.view)
                }
                offset += vis.view.frame.height
                newVisible.insert(vis, at: 0)
                visibleItemsHeight += vis.view.frame.height
                if offset >= contentOffsetY + bounds.height {
                    break
                }
            }
        }

        // removing already unneeded visible items
        oldVisible.forEach { vis in
            cellsToReuse.append(vis.view)
            hideAndRemoveFromSuperviewIfNeeded(vis.view)
        }
        let itemsCountChanged = listState.items.count != items.count
        prevProcessedOffset = contentOffsetY

        listState.visibleItems = newVisible
        // bottom drawing starts from 0 until top visible area at least (bound.height - insetTop) or above top bar (bounds.height).
        // For visible items to preserve offset after adding more items having such height is enough
        listState.itemsCanCoverScreen = visibleItemsHeight >= bounds.height - insetTop

        listState.firstVisibleItemId = listState.visibleItems.first?.item.id ?? EndlessScrollView<ScrollItem>.DEFAULT_ITEM_ID
        listState.firstVisibleItemIndex = listState.visibleItems.first?.index ?? 0
        listState.firstVisibleItemOffset = listState.visibleItems.first?.offset ?? -insetTop
        // updating the items with the last step in order to call listener with fully updated state
        listState.items = items

        estimatedContentHeight.update(contentOffset, listState, averageItemHeight, itemsCountChanged)
        scrollBarView.contentSize = CGSizeMake(bounds.width, estimatedContentHeight.virtualOverscrolledHeight)
        scrollBarView.contentOffset = CGPointMake(0, estimatedContentHeight.virtualScrollOffsetY)
        scrollBarView.isHidden = listState.visibleItems.count == listState.items.count && (listState.visibleItems.isEmpty || -listState.firstVisibleItemOffset + (listState.visibleItems.last?.offset ?? 0) + insetTop < bounds.height)

        if debug {
            println("time spent \((-start.timeIntervalSinceNow).description.prefix(5).replacingOccurrences(of: "0.000", with: "<0").replacingOccurrences(of: "0.", with: ""))")
        }
    }

    func setScrollPosition(_ index: Int, _ id: Int64, _ offset: CGFloat = 0) {
        listState.firstVisibleItemIndex = index
        listState.firstVisibleItemId = id
        listState.firstVisibleItemOffset = offset == 0 ? -bounds.height + insetTop + insetBottom : offset
    }

    func scrollToItem(_ index: Int, top: Bool = true) {
        if index >= listState.items.count || listState.isScrolling || listState.isAnimatedScrolling {
            return
        }
        if bounds.height == 0 || contentSize.height == 0 {
            scrollToItemIndexDelayed = index
            return
        }
        listState.isScrolling = true
        defer {
            listState.isScrolling = false
        }

        // just a faster way to set top item as requested index
        listState.firstVisibleItemIndex = index
        listState.firstVisibleItemId = listState.items[index].id
        listState.firstVisibleItemOffset = -bounds.height + insetTop + insetBottom
        scrollBarView.flashScrollIndicators()
        adaptItems(listState.items, false)

        var adjustedOffset = self.contentOffset.y
        var i = 0

        var upPrev = index > listState.firstVisibleItemIndex
        //let firstOrLastIndex = upPrev ? listState.visibleItems.last?.index ?? 0 : listState.firstVisibleItemIndex
        //let step: CGFloat = max(0.1, CGFloat(abs(index - firstOrLastIndex)) * scrollStepMultiplier)

        var stepSlowdownMultiplier: CGFloat = 1
        while i < 200 {
            let up = index > listState.firstVisibleItemIndex
            if upPrev != up {
                stepSlowdownMultiplier = stepSlowdownMultiplier * 0.5
                upPrev = up
            }

            // these two lines makes scrolling's finish non-linear and NOT overscroll visually when reach target index
            let firstOrLastIndex = up ? listState.visibleItems.last?.index ?? 0 : listState.firstVisibleItemIndex
            let step: CGFloat = max(0.1, CGFloat(abs(index - firstOrLastIndex)) * scrollStepMultiplier) * stepSlowdownMultiplier

            let offsetToScroll = (up ? -averageItemHeight : averageItemHeight) * step
            adjustedOffset += offsetToScroll
            if let item = listState.visibleItems.first(where: { $0.index == index }) {
                let y = if top {
                    min(estimatedContentHeight.bottomOffsetY - bounds.height, item.view.frame.origin.y - insetTop)
                } else {
                    max(estimatedContentHeight.topOffsetY - insetTop - insetBottom, item.view.frame.origin.y + item.view.bounds.height - bounds.height + insetBottom)
                }
                setContentOffset(CGPointMake(contentOffset.x, y), animated: false)
                scrollBarView.flashScrollIndicators()
                break
            }
            contentOffset = CGPointMake(contentOffset.x, adjustedOffset)
            adaptItems(listState.items, false)
            snapToContent(animated: false)
            i += 1
        }
        adaptItems(listState.items, false)
        snapToContent(animated: false)
        estimatedContentHeight.update(contentOffset, listState, averageItemHeight, true)
    }

    func scrollToItemAnimated(_ index: Int, top: Bool = true) async {
        if index >= listState.items.count || listState.isScrolling || listState.isAnimatedScrolling {
            return
        }
        listState.isAnimatedScrolling = true
        defer {
            listState.isAnimatedScrolling = false
        }
        var adjustedOffset = self.contentOffset.y
        var i = 0

        var upPrev = index > listState.firstVisibleItemIndex
        //let firstOrLastIndex = upPrev ? listState.visibleItems.last?.index ?? 0 : listState.firstVisibleItemIndex
        //let step: CGFloat = max(0.1, CGFloat(abs(index - firstOrLastIndex)) * scrollStepMultiplier)

        var stepSlowdownMultiplier: CGFloat = 1
        while i < 200 {
            let up = index > listState.firstVisibleItemIndex
            if upPrev != up {
                stepSlowdownMultiplier = stepSlowdownMultiplier * 0.5
                upPrev = up
            }

            // these two lines makes scrolling's finish non-linear and NOT overscroll visually when reach target index
            let firstOrLastIndex = up ? listState.visibleItems.last?.index ?? 0 : listState.firstVisibleItemIndex
            let step: CGFloat = max(0.1, CGFloat(abs(index - firstOrLastIndex)) * scrollStepMultiplier) * stepSlowdownMultiplier

            //println("Scrolling step \(step) \(stepSlowdownMultiplier)  index \(index) \(firstOrLastIndex)  \(index - firstOrLastIndex)  \(adjustedOffset), up \(up), i \(i)")

            let offsetToScroll = (up ? -averageItemHeight : averageItemHeight) * step
            adjustedOffset += offsetToScroll
            if let item = listState.visibleItems.first(where: { $0.index == index }) {
                let y = if top {
                    min(estimatedContentHeight.bottomOffsetY - bounds.height, item.view.frame.origin.y - insetTop)
                } else {
                    max(estimatedContentHeight.topOffsetY - insetTop - insetBottom, item.view.frame.origin.y + item.view.bounds.height - bounds.height + insetBottom)
                }
                setContentOffset(CGPointMake(contentOffset.x, y), animated: true)
                scrollBarView.flashScrollIndicators()
                break
            }
            contentOffset = CGPointMake(contentOffset.x, adjustedOffset)

            // skipping unneded relayout if this offset is already processed
            if prevProcessedOffset - contentOffset.y != 0 {
                adaptItems(listState.items, false)
                snapToContent(animated: false)
            }
            // let UI time to update to see the animated position change
            await MainActor.run {}

            i += 1
        }
        estimatedContentHeight.update(contentOffset, listState, averageItemHeight, true)
    }

    func scrollToBottom() {
        scrollToItem(0, top: false)
    }

    func scrollToBottomAnimated() {
        Task {
            await scrollToItemAnimated(0, top: false)
        }
    }

    func scroll(by: CGFloat, animated: Bool = true) {
        setContentOffset(CGPointMake(contentOffset.x, contentOffset.y + by), animated: animated)
    }

    func scrollViewShouldScrollToTop(_ scrollView: UIScrollView) -> Bool {
        if !listState.items.isEmpty {
            scrollToBottomAnimated()
        }
        return false
    }

    private func snapToContent(animated: Bool) {
        let topBlankSpace = estimatedContentHeight.height < bounds.height ? bounds.height - estimatedContentHeight.height : 0
        if topY < estimatedContentHeight.topOffsetY - topBlankSpace {
            setContentOffset(CGPointMake(0, estimatedContentHeight.topOffsetY - topBlankSpace), animated: animated)
        } else if bottomY > estimatedContentHeight.bottomOffsetY {
            setContentOffset(CGPointMake(0, estimatedContentHeight.bottomOffsetY - bounds.height), animated: animated)
        }
    }

    func offsetToBottom(_ view: UIView) -> CGFloat {
        bottomY - (view.frame.origin.y + view.frame.height)
    }

    /// If I try to .removeFromSuperview() right when I need to remove the view, it is possible to crash the app when the view was hidden in result of
    /// pressing Hide in menu on top of the revealed item within the group. So at that point the item should still be attached to the view
    func hideAndRemoveFromSuperviewIfNeeded(_ view: UIView) {
        if view.isHidden {
            // already passed this function
            return
        }
        (view as? ReusableView)?.prepareForReuse()
        view.isHidden = true
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.2) {
            if view.isHidden { view.removeFromSuperview() }
        }
    }

    /// Synchronizing both scrollViews
    func gestureRecognizer(_ gestureRecognizer: UIGestureRecognizer, shouldRecognizeSimultaneouslyWith otherGestureRecognizer: UIGestureRecognizer) -> Bool {
        true
    }

    func scrollViewDidEndDragging(_ scrollView: UIScrollView, willDecelerate decelerate: Bool) {
        if !decelerate {
            snapToContent(animated: true)
        }
    }

    override var contentOffset: CGPoint {
        get { super.contentOffset }
        set {
            var newOffset = newValue
            let topBlankSpace = estimatedContentHeight.height < bounds.height ? bounds.height - estimatedContentHeight.height : 0
            if contentOffset.y > 0 && newOffset.y < estimatedContentHeight.topOffsetY - topBlankSpace && contentOffset.y > newOffset.y {
                if !isDecelerating {
                    newOffset.y = min(contentOffset.y, newOffset.y + abs(newOffset.y - estimatedContentHeight.topOffsetY + topBlankSpace) / 1.8)
                } else {
                    DispatchQueue.main.async {
                        self.setContentOffset(newValue, animated: false)
                        self.snapToContent(animated: true)
                    }
                }
            } else if contentOffset.y > 0 && newOffset.y + bounds.height > estimatedContentHeight.bottomOffsetY && contentOffset.y < newOffset.y {
                if !isDecelerating {
                    newOffset.y = max(contentOffset.y, newOffset.y - abs(newOffset.y + bounds.height - estimatedContentHeight.bottomOffsetY) / 1.8)
                } else {
                    DispatchQueue.main.async {
                        self.setContentOffset(newValue, animated: false)
                        self.snapToContent(animated: true)
                    }
                }
            }
            super.contentOffset = newOffset
        }
    }

    private func stopScrolling() {
        let offsetYToStopAt = if abs(contentOffset.y - estimatedContentHeight.topOffsetY) < abs(bottomY - estimatedContentHeight.bottomOffsetY) {
            estimatedContentHeight.topOffsetY
        } else {
            estimatedContentHeight.bottomOffsetY - bounds.height
        }
        setContentOffset(CGPointMake(contentOffset.x, offsetYToStopAt), animated: false)
    }

    func isVisible(_ view: UIView) -> Bool {
        if view.superview == nil {
             return false
        }
        return view.frame.intersects(CGRectMake(0, contentOffset.y, bounds.width, bounds.height))
    }
}

private func println(_ text: String) {
    print("\(Date.now.timeIntervalSince1970): \(text)")
}
