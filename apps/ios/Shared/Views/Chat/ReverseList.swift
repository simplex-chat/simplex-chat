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

enum ChatScrollDirection {
    case toLatest
    case toOldest
    case none
}

enum ChatSection: CaseIterable {
    case current
    case destination
    case bottom
}

/// A List, which displays it's items in reverse order - from bottom to top
struct ReverseList<Content: View>: UIViewControllerRepresentable {
    let items: Array<ChatItem>

    @Binding var scrollState: ReverseListScrollModel.State
    @ObservedObject var sectionModel: ReverseListSectionModel
    /// Closure, that returns user interface for a given item
    let content: (ChatItem) -> Content

    let loadPage: (ChatScrollDirection, ChatSection, ChatItem?) -> Void
    
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
            case let .item(id, position, animated):
                controller.scrollToItem(id: id, position: position, animated: animated)
            case .bottom:
                controller.scroll(to: 0, position: .top, section: .bottom, shouldTryAnimate: true)
            }
        } else {
            controller.update(items: items)
        }
    }

    /// Controller, which hosts SwiftUI cells
    class Controller: UITableViewController {
        var representer: ReverseList
        private var dataSource: UITableViewDiffableDataSource<ChatSection, ChatItem>!
        private var itemCount: Int = 0
        private let updateFloatingButtons = PassthroughSubject<Void, Never>()
        private var bag = Set<AnyCancellable>()
        private var lastContentOffset: CGFloat = 0
        private var scrollDirection: ChatScrollDirection = .none
        private var requestedRange = false
        
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
            self.dataSource = UITableViewDiffableDataSource<ChatSection, ChatItem>(
                tableView: tableView
            ) { (tableView, indexPath, item) -> UITableViewCell? in
                if let section = self.dataSource.sectionIdentifier(for: indexPath.section), self.representer.scrollState == .atDestination, !self.requestedRange {
                    if self.representer.sectionModel.activeSection == section {
                        let itemCount = self.getTotalItemsInItemSection(indexPath: indexPath)
                        if self.scrollDirection == .toOldest, indexPath.item > itemCount - 8 {
                            let lastItem = self.getLastItemInItemSection(indexPath: indexPath)
                            self.requestedRange = !self.representer.sectionModel.boundaries.oldest
                            self.representer.loadPage(.toOldest, section, lastItem)
                        } else if self.scrollDirection == .toLatest, indexPath.item < 8 {
                            self.requestedRange = !self.representer.sectionModel.boundaries.latest
                            let firstItem = self.getFirstItemInItemSection(indexPath: indexPath)
                            self.representer.loadPage(.toLatest, section, firstItem)
                        }
                    }
                }
                let cell = tableView.dequeueReusableCell(withIdentifier: cellReuseId, for: indexPath)
                if #available(iOS 16.0, *) {
                    cell.contentConfiguration = UIHostingConfiguration { self.representer.content(item) }
                        .margins(.all, 0)
                        .minSize(height: 1) // Passing zero will result in system default of 44 points being used
                } else {
                    if let cell = cell as? HostingCell<Content> {
                        cell.set(content: self.representer.content(item), parent: self)
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
                        ChatView.FloatingButtonModel.shared.updateOnListChange(listState)
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
        
        /// Scrolls to a given item
        func scrollToItem(id: Int64, position: UITableView.ScrollPosition, animated: Bool) {
            if let loadedIndex = self.representer.items.firstIndex(where: { $0.id == id }) {
                let ci = representer.items[loadedIndex]
                if let indexPath = dataSource.indexPath(for: ci),
                   let section = dataSource.sectionIdentifier(for: indexPath.section) {
                    self.scroll(to: indexPath.row, position: position, section: section, shouldTryAnimate: animated)
                }
            }
        }

        /// Scrolls to Item at index path
        /// - Parameter indexPath: Item to scroll to - will scroll to beginning of the list, if `nil`
        func scroll(to index: Int?, position: UITableView.ScrollPosition, section: ChatSection, shouldTryAnimate: Bool) {
            let activeSectionBeforeScroll = representer.sectionModel.activeSection
            
            if section != activeSectionBeforeScroll {
                Task {
                    representer.sectionModel.changeActiveSection(section)
                }
            }
            var animated = false
            if #available(iOS 16.0, *) {
                animated = shouldTryAnimate
            }
            if let index, let sectionIndex = dataSource.index(for: section), tableView.numberOfRows(inSection: sectionIndex) != 0 {
                tableView.scrollToRow(
                    at: IndexPath(row: index, section: sectionIndex),
                    at: position,
                    animated: animated
                )
            } else {
                tableView.setContentOffset(
                    CGPoint(x: .zero, y: -InvertedTableView.inset),
                    animated: animated
                )
            }
            Task {
                representer.scrollState = .atDestination
                if activeSectionBeforeScroll != section {
                    representer.sectionModel.maybeDropSection(activeSectionBeforeScroll)
                }
            }
        }

        func update(items: [ChatItem]) {
            requestedRange = false
            var snapshot = NSDiffableDataSourceSnapshot<ChatSection, ChatItem>()
            let sections = self.representer.sectionModel.getSectionsOrdered()
            let itemsBySection = self.itemsBySection(items: items)
            snapshot.appendSections(sections)
            sections.forEach { sec in
                if let sectionItems = itemsBySection[sec] {
                    snapshot.appendItems(sectionItems, toSection: sec)
                }
            }
            dataSource.defaultRowAnimation = .none
            dataSource.apply(
                snapshot,
                animatingDifferences: itemCount != 0 && abs(items.count - itemCount) == 1
            )
            // Sets content offset on initial load
            if itemCount == 0 {
                tableView.setContentOffset(
                    CGPoint(x: 0, y: -InvertedTableView.inset),
                    animated: false
                )
            }
            itemCount = items.count
            updateFloatingButtons.send()
        }

        override func scrollViewDidScroll(_ scrollView: UIScrollView) {
            let currentOffset = scrollView.contentOffset.y
            if currentOffset > lastContentOffset {
                scrollDirection = .toOldest
            } else if currentOffset < lastContentOffset {
                scrollDirection = .toLatest
            } else {
                scrollDirection = .none
            }
            lastContentOffset = currentOffset
            updateFloatingButtons.send()
        }

        func getListState() -> ListState? {
            if let visibleRows = tableView.indexPathsForVisibleRows,
                visibleRows.last?.item ?? 0 < representer.items.count {
                let scrollOffset: Double = tableView.contentOffset.y + InvertedTableView.inset
            
                let topItemDate: Date? =
                    if let lastVisible = visibleRows.last(where: { isVisible(indexPath: $0) }),
                       let cI = self.dataSource.itemIdentifier(for: lastVisible) {
                        cI.meta.itemTs
                    } else {
                        nil
                    }
                let bottomItemId: ChatItem.ID? =
                    if let firstVisible = visibleRows.first(where: { isVisible(indexPath: $0) }),
                       let cI = self.dataSource.itemIdentifier(for: firstVisible) {
                        cI.id
                    } else {
                        nil
                    }
                return (scrollOffset: scrollOffset, topItemDate: topItemDate, bottomItemId: bottomItemId)
            }
            return nil
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
        
        private func getTotalItemsInItemSection(indexPath: IndexPath) -> Int {
            return self.tableView.numberOfRows(inSection: indexPath.section)
        }
        
        private func getLastItemInItemSection(indexPath: IndexPath) -> ChatItem? {
            let numberOfRows = self.getTotalItemsInItemSection(indexPath: indexPath)
            
            return if numberOfRows > 0,
                      let lastItem = self.dataSource.itemIdentifier(for: IndexPath(row: numberOfRows - 1, section: indexPath.section)) {
                lastItem
            } else {
                nil
            }
        }
        
        private func getFirstItemInItemSection(indexPath: IndexPath) -> ChatItem? {
            let firstIndexPath = IndexPath(item: 0, section: indexPath.section)
            return if let firstItem = self.dataSource.itemIdentifier(for: firstIndexPath) {
                firstItem
            } else {
                nil
            }
        }
        
        private func itemsBySection(items: [ChatItem]) -> [ChatSection: [ChatItem]] {
            let itemsBySection = items.reduce(into: [ChatSection: [ChatItem]]()) { result, ci in
                if let sec = self.representer.sectionModel.getItemSection(ci.id) {
                    result[sec, default: []].append(ci)
                }
            }
            
            return itemsBySection
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

typealias ListState = (
    scrollOffset: Double,
    topItemDate: Date?,
    bottomItemId: ChatItem.ID?
)

/// Manages ``ReverseList`` scrolling
class ReverseListScrollModel: ObservableObject {
    /// Represents Scroll State of ``ReverseList``
    enum State: Equatable {
        enum Destination: Equatable {
            case nextPage
            case item(ChatItem.ID, UITableView.ScrollPosition, Bool)
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

    func scrollToItem(id: ChatItem.ID, position: UITableView.ScrollPosition? = .bottom, animated: Bool = true) {
        state = .scrollingTo(.item(id, position ?? .bottom, animated))
    }
}

struct SectionBoundaryReached {
    var oldest: Bool
    var latest: Bool
}

/// Manages ``ReverseList`` sections
class ReverseListSectionModel: ObservableObject {
    static let shared = ReverseListSectionModel()
    static let MAX_SECTION_SIZE = 500
    static let MIN_SECTION_SIZE = 200
    @Published private(set) var activeSection: ChatSection = .bottom
    @Published var boundaries = SectionBoundaryReached(oldest: false, latest: false)
    @Published private var itemSection: [Int64: ChatSection] = [:]
    @Published private var sections = Set<ChatSection>()

    func getItemSection(_ id: Int64) -> ChatSection? {
        return itemSection[id]
    }
    
    func changeActiveSection(_ section: ChatSection) {
        activeSection = section
        boundaries.latest = false
        boundaries.oldest = false
    }
    
    func maybeDropSection(_ section: ChatSection) {
        if section == .bottom { return }
        let im = ItemsModel.shared
        
        sections.remove(section)
        im.reversedChatItems = im.reversedChatItems.filter { it in itemSection[it.id] != section }
    }
    
    private func activeSectionItemsCount() -> Int {
        // TODO: check
        return itemSection.values.filter { $0 == self.activeSection }.count
    }
    
    private func splitSection(originalSection: ChatSection, newSection: ChatSection) {
        var originalSectionCount = 0
        let im = ItemsModel.shared

        im.reversedChatItems.forEach { it in
            if itemSection[it.id] == originalSection {
                if originalSectionCount < ReverseListSectionModel.MIN_SECTION_SIZE {
                    originalSectionCount += 1
                } else {
                    itemSection[it.id] = newSection
                }
            }
        }
    }
    
    private func trimSection(scrollDirection: ChatScrollDirection, section: ChatSection) {
        let im = ItemsModel.shared
        var i = scrollDirection == .toLatest ? 0 : im.reversedChatItems.count - 1
        let moveToNextIndex: () -> Bool = {
            if scrollDirection == .toLatest {
                if i < im.reversedChatItems.count - 1 {
                    i += 1
                    return false
                } else {
                    return true
                }
            } else {
                if i > 0 {
                    i -= 1
                    return false
                } else {
                    return true
                }
            }
        }
        var sectionCount = 0
        var completed = false
        var toRemoveIndexes = IndexSet()
        
        while (!completed) {
            let it = im.reversedChatItems[i]
            if itemSection[it.id] == section {
                if sectionCount >= ReverseListSectionModel.MAX_SECTION_SIZE {
                    toRemoveIndexes.insert(i)
                    itemSection.removeValue(forKey: it.id)
                } else {
                    sectionCount += 1
                }
            }
            completed = moveToNextIndex()
        }
        
        if (!toRemoveIndexes.isEmpty) {
            im.reversedChatItems.remove(atOffsets: toRemoveIndexes)
        }
    }
    
    func manageActiveSection(scrollDirection: ChatScrollDirection) {
        let activeSectionCount = activeSectionItemsCount()

        switch self.activeSection {
        case .bottom:
            if activeSectionCount > ReverseListSectionModel.MAX_SECTION_SIZE {
                if scrollDirection == .toLatest {
                boundaries.oldest = false
                self.trimSection(scrollDirection: scrollDirection, section: self.activeSection)
                } else {
                    splitSection(originalSection: .bottom, newSection: .current)
                    changeActiveSection(.current)
                    sections.insert(.current)
                }
            }
            break;
        default:
            if activeSectionCount > ReverseListSectionModel.MAX_SECTION_SIZE {
                if scrollDirection == .toLatest {
                    boundaries.oldest = false
                } else {
                    boundaries.latest = false
                }
                self.trimSection(scrollDirection: scrollDirection, section: self.activeSection)
            }
        }
    }
    
    func getSectionsOrdered() -> [ChatSection] {
        var orderedSections: [ChatSection] = [.bottom]
        
        if (sections.contains(.current)) {
            orderedSections.append(.current)
        }
        
        if (sections.contains(.destination)) {
            orderedSections.append(.destination)
        }
        
        return orderedSections
    }
    
    func resetSections(items: [ChatItem]) {
        itemSection = Dictionary(uniqueKeysWithValues: items.map { ($0.id, .bottom) })
    }
    
    func handleSectionInsertion(candidateSection: ChatSection, reversedPage: [ChatItem], allItems: [ChatItem]) -> [ChatItem] {
        var reversedPageToAppend = Array<ChatItem>()
        var targetSection = candidateSection
        
        if let sectionIntersectionItem = reversedPage.first(where: { itemSection[$0.id] != nil && itemSection[$0.id] != candidateSection }) {
            let sectionToDrop = itemSection[sectionIntersectionItem.id] == .bottom ? candidateSection : (itemSection[sectionIntersectionItem.id] ?? .destination)
            targetSection = itemSection[sectionIntersectionItem.id] == .bottom ? .bottom : candidateSection
            itemSection = itemSection.mapValues { section in
                section == sectionToDrop ? targetSection : section
            }
            if (sectionToDrop != targetSection) {
                sections.remove(sectionToDrop)
            }
        }
        
        reversedPage.forEach { ci in
            if itemSection[ci.id] == nil {
                reversedPageToAppend.append(ci)
                itemSection[ci.id] = targetSection
            }
        }
        sections.insert(targetSection)
        self.activeSection = targetSection
        
        return reversedPageToAppend
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
}
