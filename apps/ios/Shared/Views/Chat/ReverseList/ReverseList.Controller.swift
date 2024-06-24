//
//  ReverseList.Controller.swift
//  SimpleX (iOS)
//
//  Created by Levitating Pineapple on 12/06/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import Combine
import SwiftUI

extension ReverseList {
    /// Controller, which hosts SwiftUI cells
    class Controller: UITableViewController {
        private enum Section { case main }
        private let representer: ReverseList
        private var dataSource: UITableViewDiffableDataSource<Section, Item>!
        private var itemCount: Int = .zero
        private var isNearBottom = PassthroughSubject<Bool, Never>()
        private var bag = Set<AnyCancellable>()

        init(representer: ReverseList) {
            self.representer = representer
            super.init(style: .plain)

            // 1. Style
            tableView.separatorStyle = .none
            tableView.transform = .verticalFlip

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
            self.dataSource = UITableViewDiffableDataSource<Section, Item>(
                tableView: tableView
            ) { (tableView, indexPath, item) -> UITableViewCell? in
                self.representer.loadedCell(indexPath)
                let cell = tableView.dequeueReusableCell(withIdentifier: cellReuseId, for: indexPath)
                if #available(iOS 16.0, *) {
                    cell.contentConfiguration = UIHostingConfiguration { self.representer.content(item) }
                        .margins(.all, .zero)
                        .minSize(height: 4) // Passing zero will result in system default of 44 points being used
                } else {
                    if let cell = cell as? HostingCell<Content> {
                        cell.set(content: self.representer.content(item), parent: self)
                    } else {
                        fatalError("Unexpected Cell Type for: \(item)")
                    }
                }
                cell.transform = .verticalFlip
                cell.selectionStyle = .none
                return cell
            }

            // 4. Manage Scroll State
            isNearBottom
                .removeDuplicates()
                .sink { isNearBottom in
                    Task(priority: .userInitiated) {
                        representer.scroll = .isNearBottom(isNearBottom)
                    }
                }
                .store(in: &bag)
        }

        @available(*, unavailable)
        required init?(coder: NSCoder) { fatalError() }

        override func scrollViewDidScroll(_ scrollView: UIScrollView) {
            isNearBottom.send(scrollView.contentOffset.y < scrollView.frame.height)
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
        }

        /// Scrolls to Item at index path
        /// - Parameter indexPath: Item to scroll to - will scroll to beginning of the list, if `nil`
        func scroll(to index: Int?) {
            if let index {
                tableView.scrollToRow(
                    at: IndexPath(row: index, section: .zero),
                    at: .middle,
                    animated: true
                )
            }
        }

        func update(items: Array<Item>) {
            var snapshot = NSDiffableDataSourceSnapshot<Section, Item>()
            snapshot.appendSections([.main])
            snapshot.appendItems(items)
            dataSource.defaultRowAnimation = .none
            var animatingDifferences = false
            if #available(iOS 16.0, *) {
                animatingDifferences = (items.count - itemCount) < Timeline.pageLoad
            }
            dataSource.apply(snapshot, animatingDifferences: animatingDifferences)
            itemCount = items.count
        }
    }
}

fileprivate let cellReuseId = "hostingCell"

fileprivate extension CGAffineTransform {
    /// Transform that vertically flips the view, preserving it's location
    static let verticalFlip = CGAffineTransform(scaleX: 1, y: -1)
}
