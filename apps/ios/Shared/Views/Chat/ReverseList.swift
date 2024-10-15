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
    case main
    case destination
    case bottom
}

/// A List, which displays it's items in reverse order - from bottom to top
struct ReverseList<Content: View>: UIViewControllerRepresentable {
    let items: Array<ChatItem>

    @Binding var scrollState: ReverseListScrollModel.State
    @Binding var itemSection: Dictionary<Int64, ChatSection>
    @Binding var activeSection: ChatSection
    /// Closure, that returns user interface for a given item
    let content: (ChatItem) -> Content

    let loadPage: (ChatScrollDirection, ChatSection, ChatItem?) -> Void
    let loadItemsAround: (Int64) -> Void
    
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
                controller.update(items: items)
                controller.scrollToItem(id: id)
            case .bottom:
                controller.scroll(to: 0, position: .top, section: .bottom)
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
                if let section = self.dataSource.sectionIdentifier(for: indexPath.section) {
                    let itemCount = self.getTotalItemsInItemSection(indexPath: indexPath)
                    if self.representer.activeSection == section, indexPath.item > itemCount - 8, itemCount > 8 {
                        let lastItem = self.getLastItemInItemSection(indexPath: indexPath)
                        self.representer.loadPage(.toOldest, section, lastItem)
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
        
        /// Scrolls up
        func scrollToItem(id: Int64) {
            if let loadedIndex = self.representer.items.firstIndex(where: { $0.id == id }) {
                let ci = representer.items[loadedIndex]
                if let indexPath = dataSource.indexPath(for: ci),
                   let section = dataSource.sectionIdentifier(for: indexPath.section) {
                    self.scroll(to: indexPath.row, position: .bottom, section: section)
                }
            } else {
                Task {
                    if (self.representer.itemSection[id] == nil) {
                        self.representer.activeSection = .destination
                        self.representer.loadItemsAround(id)
                    }
                }
            }
        }

        /// Scrolls to Item at index path
        /// - Parameter indexPath: Item to scroll to - will scroll to beginning of the list, if `nil`
        func scroll(to index: Int?, position: UITableView.ScrollPosition, section: ChatSection) {
            var animated = false
            if #available(iOS 16.0, *) {
                animated = true
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
            Task { representer.scrollState = .atDestination }
        }

        func update(items: [ChatItem]) {
            var snapshot = NSDiffableDataSourceSnapshot<ChatSection, ChatItem>()
            var itemsBySection: Dictionary<ChatSection, [ChatItem]> = Dictionary()
            items.forEach { ci in
                var section: ChatSection = .bottom
                if let sec = representer.itemSection[ci.id] {
                    section = sec
                }
                
                if var existingItems = itemsBySection[section] {
                    existingItems.append(ci)
                    itemsBySection[section] = existingItems
                } else {
                    itemsBySection[section] = [ci]
                }
            }
            snapshot.appendSections([.bottom, .destination])
            if let bottomItems = itemsBySection[.bottom] {
                snapshot.appendItems(bottomItems, toSection: .bottom)
            }
            if let destinationItems = itemsBySection[.destination] {
                snapshot.appendItems(destinationItems, toSection: .destination)
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
            case item(ChatItem.ID)
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
