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

        fileprivate init(scrollView: EndlessScrollView<ScrollItem> = EndlessScrollView(frame: .zero), content: @escaping (Int, ScrollItem) -> Content) {
            self.scrollView = scrollView
            self.content = content
            super.init(nibName: nil, bundle: nil)
            self.view = scrollView
            scrollView.createCell = createCell
            scrollView.updateCell = updateCell
        }

        required init?(coder: NSCoder) { fatalError() }

        private func createCell(_ index: Int, _ items: [ScrollItem], _ cellsToReuse: inout [UITableViewCell]) -> UITableViewCell {
            let item = items[index]
            println("LALAL ASK FOR CELL0 \(index)  \(cellsToReuse.count)")
            let cell: UITableViewCell
            if #available(iOS 16.0, *), false {
                let c = cellsToReuse.isEmpty ? UITableViewCell() : cellsToReuse.removeFirst()
                c.contentConfiguration = UIHostingConfiguration { self.content(index, item) }
                    .margins(.all, 0)
                    .minSize(height: 1) // Passing zero will result in system default of 44 points being used
                cell = c
            } else {
                let c = cellsToReuse.isEmpty ? HostingCell<Content>() : cellsToReuse.removeFirst() as! HostingCell<Content>
                c.set(content: self.content(index, item), parent: self)
                cell = c
            }
            cell.isHidden = false
            cell.backgroundColor = .clear
            let size = cell.systemLayoutSizeFitting(CGSizeMake(scrollView.bounds.width, CGFloat.greatestFiniteMagnitude))
            cell.frame.size.width = scrollView.bounds.width
            cell.frame.size.height = size.height
            //cell.sizeToFit()
            //cell.setNeedsLayout()
            //cell.layoutIfNeeded()
            //cell.autoresizingMask = UIView.AutoresizingMask(rawValue: UIView.AutoresizingMask.flexibleTopMargin.rawValue | UIView.AutoresizingMask.flexibleWidth.rawValue)
            cell.autoresizingMask = UIView.AutoresizingMask.flexibleWidth
            //cell.setContentHuggingPriority(.defaultLow, for: .vertical)
            println("LALAL ASK FOR CELL1 \(index) \(cell.frame) \(size)")
            return cell
        }

        private func updateCell(cell: UITableViewCell, _ index: Int, _ items: [ScrollItem]) {
            let item = items[index]
            if #available(iOS 16.0, *), false {
                cell.contentConfiguration = UIHostingConfiguration { self.content(index, item) }
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
            cell.layoutIfNeeded()
            println("LALAL ASK FOR CELL RELOAD \(index) \(cell.frame) \(size)")
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
    var averageItemHeight: CGFloat = 20

    /// This is used as a multiplier for difference between current index and scrollTo index using [averageItemHeight] as well. Increase it to get faster speed
    var scrollStepMultiplier: CGFloat = 0.2

    /// Adds content padding to top
    var insetTop: CGFloat = 100

    /// Adds content padding to bottom
    var insetBottom: CGFloat = 100

    /// The second scroll view that is used only for purpose of displaying scroll bar with made-up content size and scroll offset that is gathered from main scroll view, see [estimatedContentHeight]
    let scrollBarView: UIScrollView = UIScrollView(frame: .zero)

    var cellsToReuse: [UITableViewCell] = []

    /// Enable debug to see hundreds of logs
    var debug: Bool = true

    var createCell: (Int, [ScrollItem], inout [UITableViewCell]) -> UITableViewCell? = { _, _, _ in nil }
    var updateCell: (UITableViewCell, Int, [ScrollItem]) -> Void = { cell, _, _ in }


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

        var lastVisibleItemIndex: Int {
            visibleItems.last?.index ?? 0
        }

        /// Whether there is scroll to item in progress or not
        var isScrolling: Bool = false

        override init() {
            super.init()
        }
    }

    class VisibleItem {
        let index: Int
        let item: ScrollItem
        let view: UITableViewCell
        var offset: CGFloat

        init(index: Int, item: ScrollItem, view: UITableViewCell, offset: CGFloat) {
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

        var indexBasedTopOffsetY: CGFloat = 0

        /// How much distance were overscolled on top which often means to show sticky scrolling that should scroll back to real position after a users finishes dragging the scrollView
        var overscrolledTop: CGFloat = 0

        /// Adds content padding to bottom and top
        var inset: CGFloat = 100

        /// Ability to disable adjusting size of vistual scroll area
        var disabledSizeAdjusting: Bool = true

        /// In order to see a less laggy scroll bar when a user scrolls to items with unknown size, this is used for adjusting scrollable height
        var sizeChanges: CGFloat = 0

        /// Estimated height of the contents of scroll view
        var height: CGFloat {
            get { topOffsetY - bottomOffsetY }
        }

        /// Estimated height of the contents of scroll view + distance of overscrolled effect. It's only updated when number of item changes to prevent jumping of scroll bar
        var virtualOverscrolledHeight: CGFloat {
            get {
                bottomOffsetY - topOffsetY - overscrolledTop - sizeChanges - inset * 2
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
                indexBasedTopOffsetY = 0
                overscrolledTop = 0
                return
            }
            let wasTopOffset = topOffsetY
            let wasBottomOffset = bottomOffsetY
            topOffsetY = last.view.frame.origin.y - CGFloat(listState.totalItemsCount - last.index - 1) * averageItemHeight - self.inset
            bottomOffsetY = first.view.frame.origin.y + first.view.bounds.height + CGFloat(first.index) * averageItemHeight + self.inset
            overscrolledTop = max(0, last.index == listState.totalItemsCount - 1 ? last.view.frame.origin.y - contentOffset.y : 0)

            if updateStaleHeight || disabledSizeAdjusting {
                sizeChanges = 0
            } else if wasTopOffset != 0 {
                sizeChanges -= wasTopOffset - topOffsetY
                sizeChanges -= wasBottomOffset - bottomOffsetY
            }

            virtualScrollOffsetY = contentOffset.y - topOffsetY

            println("LALAL TOP \(topOffsetY) bottom \(bottomOffsetY) offset \(virtualScrollOffsetY), diff: \(topOffsetY - wasTopOffset) \(bottomOffsetY - wasBottomOffset)")
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
        }
        scrollBarView.frame = CGRectMake(frame.width - 10, self.insetTop, 10, frame.height - self.insetTop - self.insetBottom)
        oldScreenHeight = newScreenHeight
        adaptItems(listState.items)
    }

    private func setup() {
        //backgroundColor = UIColor.blue
        var s = frame.size
        println("LALAL SIZE \(contentSize) \(frame)")
        s.height = initialOffset * 2
        contentSize = s
        prevProcessedOffset = initialOffset
        contentOffset = CGPointMake(0, initialOffset)
        
        showsVerticalScrollIndicator = false
        scrollBarView.showsHorizontalScrollIndicator = false
        panGestureRecognizer.delegate = self
        addGestureRecognizer(scrollBarView.panGestureRecognizer)
        superview!.addSubview(scrollBarView)
    }

    func updateItems(_ items: [ScrollItem]) {
        if !Thread.isMainThread {
            fatalError("Use main thread to update items")
        }
        if bounds.height == 0 {
            self.listState.items = items
            // this function requires to have valid bounds and it will be called again once it has them
            return
        }
        adaptItems(items)
    }

    func adaptItems(_ items: [ScrollItem], overridenOffset: CGFloat? = nil) {
        let start = Date.now
        // let cell = createCell(i, items, &cellsToReuse)!
        // special case when everything was removed
        if items.isEmpty {
            listState.visibleItems.forEach { item in item.view.removeFromSuperview() }
            listState.visibleItems = []
            listState.firstVisibleItemId = EndlessScrollView<ScrollItem>.DEFAULT_ITEM_ID
            listState.firstVisibleItemIndex = 0
            listState.firstVisibleItemOffset = -insetTop

            estimatedContentHeight.update(contentOffset, listState, averageItemHeight, true)
            scrollBarView.contentSize = .zero
            scrollBarView.contentOffset = .zero

            prevProcessedOffset = contentOffset.y
            self.listState.items = items
            return
        }

        let contentOffsetY = overridenOffset ?? contentOffset.y

        var newVisible: [VisibleItem] = []
        let offsetsDiff = contentOffsetY - prevProcessedOffset
        if debug {
            println("LALAL OFFSET DIFF \(offsetsDiff)")
        }

        var shouldBeFirstVisible = items.firstIndex(where: { item in item.id == listState.firstVisibleItemId as! ScrollItem.ID }) ?? 0

        var allowOneMore = false
        var nextOffsetY: CGFloat = 0
        // building list of visible items starting from the first one that should be visible
        for i in shouldBeFirstVisible ..< items.count {
            let item = items[i]
            let visibleIndex = listState.visibleItems.firstIndex(where: { vis in vis.item.id == item.id })
            let visible: VisibleItem?
            if let visibleIndex {
                let v = listState.visibleItems[visibleIndex]
                if v.item.hashValue != item.hashValue {
                    updateCell(v.view, i, items)
                }
                visible = v
            } else {
                visible = nil
            }

            if shouldBeFirstVisible == i {
                if let vis = visible, let visibleIndex {
                    let oldHeight = vis.view.frame.height
                    vis.view.frame.origin.y += oldHeight - vis.view.frame.height
//                    vis.view.frame.origin.y -= offsetsDiff
                    //vis.offset -= offsetsDiff
                    // the fist visible item previously is hidden now, remove it and move on
                    if !isVisible(vis.view) {
                        shouldBeFirstVisible += 1
                        //firstVisibleItemOffset -= vis.view.frame.height
                        //vis.view.removeFromSuperview()
                        hideAndRemoveFromSuperviewIfNeeded(vis.view)
                        listState.visibleItems.remove(at: visibleIndex)
                        cellsToReuse.append(vis.view)
                        continue
                    }
                }
                let vis: VisibleItem
                if let visible {
                    vis = VisibleItem(index: i, item: item, view: visible.view, offset: offsetToBottom(visible.view))
                } else {
                    let cell = createCell(i, items, &cellsToReuse)!
                    cell.frame.origin.y = bottomY + listState.firstVisibleItemOffset - cell.frame.height
                    vis = VisibleItem(index: i, item: item, view: cell, offset: offsetToBottom(cell))
                }
                if vis.view.superview == nil {
                    addSubview(vis.view)
                }
                newVisible.append(vis)
                nextOffsetY = vis.view.frame.origin.y
//                println("LALAL ORIGIN0 \(vis.view.frame.origin.y)")
            } else {
                // LALAL TOO MANY CREATECELL CALLS
                let vis: VisibleItem
                if let visible {
                    vis = VisibleItem(index: i, item: item, view: visible.view, offset: offsetToBottom(visible.view))
                } else {
                    let cell = createCell(i, items, &cellsToReuse)!
                    vis = VisibleItem(index: i, item: item, view: cell, offset: offsetToBottom(cell))
                }
                nextOffsetY -= vis.view.frame.height
                vis.view.frame.origin.y = nextOffsetY
                if vis.view.superview == nil {
                    addSubview(vis.view)
                }
                newVisible.append(vis)
                //println("LALAL ORIGIN1 \(vis.view.frame.origin.y)")
            }
            if abs(nextOffsetY) < contentOffsetY && !allowOneMore {
                break
            } else if abs(nextOffsetY) < contentOffsetY {
                allowOneMore = false
            }
        }
        if let firstVisible = newVisible.first, firstVisible.view.frame.origin.y + firstVisible.view.frame.height < contentOffsetY + bounds.height, firstVisible.index > 0 {
            var offset: CGFloat = firstVisible.view.frame.origin.y + firstVisible.view.frame.height
            let index = firstVisible.index
            for i in stride(from: index - 1, through: 0, by: -1) {
                let item = items[i]
                let visible = listState.visibleItems.first(where: { vis in vis.item.id as! ScrollItem.ID == item.id })
                let vis: VisibleItem
                if let visible {
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
                if offset > contentOffsetY + bounds.height {
                    break
                }
            }
        }


        //let cellsToReuseCount = cellsToReuse.count
        //cellsToReuse.forEach { cell in cell.removeFromSuperview() }
        cellsToReuse.forEach { cell in hideAndRemoveFromSuperviewIfNeeded(cell) }
        // removing already unneeded visible items
        listState.visibleItems.forEach { vis in
            if !newVisible.contains(where: { newVis in newVis.item.id.hashValue == vis.item.id.hashValue }) {
                //vis.view.removeFromSuperview()
                hideAndRemoveFromSuperviewIfNeeded(vis.view)
                cellsToReuse.append(vis.view)
            }
        }

        let itemsCountChanged = listState.items.count != items.count
        prevProcessedOffset = contentOffsetY

        listState.visibleItems = newVisible
        listState.items = items

        listState.firstVisibleItemId = listState.visibleItems.first?.item.id ?? EndlessScrollView<ScrollItem>.DEFAULT_ITEM_ID
        listState.firstVisibleItemIndex = listState.visibleItems.first?.index ?? 0
        listState.firstVisibleItemOffset = listState.visibleItems.first?.offset ?? -insetTop

        estimatedContentHeight.update(contentOffset, listState, averageItemHeight, itemsCountChanged)

        if debug {
            println("LALAL CURRENTOFFSET \(estimatedContentHeight.virtualScrollOffsetY)  \(estimatedContentHeight.virtualOverscrolledHeight)")
        }
        scrollBarView.contentSize = CGSizeMake(bounds.width, estimatedContentHeight.virtualOverscrolledHeight)
        scrollBarView.setContentOffset(CGPointMake(0, estimatedContentHeight.virtualScrollOffsetY), animated: false)

        if debug {
            //println("LALAL VISIBLE \(newVisible.map({ item in (item.index, item.offset) })), cellsToReuse \(cellsToReuseCount), time spent \((-start.timeIntervalSinceNow).description.prefix(5).replacingOccurrences(of: "0.000", with: "<0").replacingOccurrences(of: "0.", with: ""))")
            println("time spent \((-start.timeIntervalSinceNow).description.prefix(5).replacingOccurrences(of: "0.000", with: "<0").replacingOccurrences(of: "0.", with: ""))")
        }
    }

    func scrollToItem(_ index: Int, animated: Bool) async {
        if index >= listState.items.count || listState.isScrolling {
            return
        }
        listState.isScrolling = true
        defer {
            listState.isScrolling = false
        }
        if !animated {
            // just a faster way to set top item as requested index
            listState.firstVisibleItemIndex = index
            listState.firstVisibleItemId = listState.items[index].id
            listState.firstVisibleItemOffset = -bounds.height + insetTop + insetBottom
            scrollBarView.flashScrollIndicators()
            adaptItems(listState.items)
            //return
        }
        var adjustedOffset = self.contentOffset.y
        var i = 0
        let up = index > listState.firstVisibleItemIndex
        let firstOrLastIndex = up ? listState.visibleItems.last?.index ?? 0 : listState.firstVisibleItemIndex
        while adjustedOffset >= estimatedContentHeight.topOffsetY - bounds.height && adjustedOffset <=
                estimatedContentHeight.bottomOffsetY + bounds.height {
            println("LALAL first \(firstOrLastIndex) \(up) \(adjustedOffset)")
            let step: CGFloat = max(0.1, CGFloat(abs(index - firstOrLastIndex)) * scrollStepMultiplier)
            adjustedOffset += (up ? -averageItemHeight : averageItemHeight) * step
            if let item = listState.visibleItems.first(where: { $0.index == index }) {
                if item.view.frame.origin.y + (up ? -insetTop : 0) <= adjustedOffset || i > 0 {
                    // the requested item became visible, scroll to it
                    setContentOffset(CGPointMake(contentOffset.x, min(estimatedContentHeight.bottomOffsetY - bounds.height + (up ? -insetTop : 0), item.view.frame.origin.y + (up ? -insetTop : 0))), animated: animated)
                    //contentOffset = CGPointMake(contentOffset.x, min(estimatedContentHeight.bottomOffsetY - bounds.height + (up ? -insetTop : 0), item.view.frame.origin.y + (up ? -insetTop : 0)))
                } else {
                    // the requested item was visible initially, scroll to it animated
                    setContentOffset(CGPointMake(contentOffset.x, min(estimatedContentHeight.bottomOffsetY - bounds.height + (up ? -insetTop : 0), item.view.frame.origin.y + (up ? -insetTop : 0))), animated: animated)
                }
                scrollBarView.flashScrollIndicators()
                break
            }
            //println("LALAL FIRST \(self.listState.visibleItems.first?.index ?? 0)  \(self.listState.visibleItems.first?.offset ?? 0)  \(was)  \(self.contentOffset.y) \(firstOrLastIndex)")
            //            if let first = visibleItems.first, first.offset + first.view.bounds.height >= 0 {
            //                break
            //            }
            contentOffset = CGPointMake(contentOffset.x, adjustedOffset)
            if animated {
                // skipping unneded relayout if this offset is already processed
                if prevProcessedOffset - contentOffset.y != 0 {
                    adaptItems(listState.items)
                }
                // let UI time to update to see the animated position change
                try? await Task.sleep(nanoseconds: 10_000000)
            }
            i += 1
        }
        estimatedContentHeight.update(contentOffset, listState, averageItemHeight, true)
    }

    private func snapToContent() {
        if bottomY > estimatedContentHeight.bottomOffsetY {
            setContentOffset(CGPointMake(0, estimatedContentHeight.bottomOffsetY - bounds.height), animated: true)
        } else if topY < estimatedContentHeight.topOffsetY {
            setContentOffset(CGPointMake(0, estimatedContentHeight.topOffsetY), animated: true)
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
        view.isHidden = true
        println("LALAL IS HIDDEN DISAP0")
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
            println("LALAL IS HIDDEN DISAP1 \(view.isHidden)")
            if view.isHidden { view.removeFromSuperview() }
        }
    }

    /// Synchronizing both scrollViews
    func gestureRecognizer(_ gestureRecognizer: UIGestureRecognizer, shouldRecognizeSimultaneouslyWith otherGestureRecognizer: UIGestureRecognizer) -> Bool {
        true
    }

//    override var contentInsetAdjustmentBehavior: UIScrollView.ContentInsetAdjustmentBehavior {
//        get { .never }
//        set { }
//    }
//
//    override var contentInset: UIEdgeInsets {
//        get { insets }
//        set { }
//    }
//
//    override var adjustedContentInset: UIEdgeInsets {
//        insets
//    }

//    func scrollViewDidScroll(_ scrollView: UIScrollView) {
//        //adaptItems(items)
//    }

    func scrollViewDidEndDragging(_ scrollView: UIScrollView, willDecelerate decelerate: Bool) {
        if !decelerate {
            snapToContent()
        }
    }

    override var contentOffset: CGPoint {
        get { super.contentOffset }
        set {
            var newOffset = newValue
            if contentOffset.y > 0 && newOffset.y < estimatedContentHeight.topOffsetY && contentOffset.y > newOffset.y {
                //println("LALAL OVERTOP \(estimatedContentHeight.topOffsetY - newOffset.y)")
                if !isDecelerating {
                    newOffset.y = min(contentOffset.y, newOffset.y + abs(newOffset.y - estimatedContentHeight.topOffsetY) / 1.8)
                } else {
                    DispatchQueue.main.async {
                        self.setContentOffset(newValue, animated: false)
                        self.snapToContent()
                    }
                }
            } else if contentOffset.y > 0 && newOffset.y + bounds.height > estimatedContentHeight.bottomOffsetY && contentOffset.y < newOffset.y {
                //println("LALAL OVERBOTTOM \(estimatedContentHeight.bottomOffsetY - newOffset.y - bounds.height)")
                if !isDecelerating {
                    newOffset.y = max(contentOffset.y, newOffset.y - abs(newOffset.y + bounds.height - estimatedContentHeight.bottomOffsetY) / 1.8)
                } else {
                    DispatchQueue.main.async {
                        self.setContentOffset(newValue, animated: false)
                        self.snapToContent()
                    }
                }
                //println("LALAL OVERBOTTOM was \(contentOffset.y) now \(newOffset.y)")
            }
            //println("LALAL SETTING OFFSET \(newValue.y)")
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
        let res = view.frame.intersects(CGRectMake(0, contentOffset.y, bounds.width, bounds.height))
        if debug {
            println("LALAL CELL FRAME \(view.frame.debugDescription) \(CGRectMake(0, contentOffset.y, bounds.width, bounds.height).debugDescription)  res \(res)")
        }
        return res
    }
}

private func println(_ text: String) {
    print("\(Date.now.timeIntervalSince1970): \(text)")
}
