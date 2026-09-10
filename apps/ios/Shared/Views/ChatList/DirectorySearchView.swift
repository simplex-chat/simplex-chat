//
//  DirectorySearchView.swift
//  SimpleX
//
//  Created by spaced4ndy on 13.08.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

// Results of searching the directory over the service RPC. They are not chats and are never
// persisted: they live as long as the search text does.
@MainActor
class DirectorySearchModel: ObservableObject {
    @Published private(set) var entries: [DirectorySearchEntry] = []
    @Published private(set) var loading = false
    @Published private(set) var failed = false
    // set once a search has actually run, so the empty state can tell "not searched yet" from
    // "searched and found nothing"
    @Published private(set) var searched = false

    private var cursor: JSONValue? = nil
    private var searchedText = ""
    // bumped on every reset, so a reply that arrives after the text, profile or host changed
    // cannot repopulate a list the user has moved on from
    private var generation = 0

    var hasMore: Bool { cursor != nil }

    func reset() {
        generation += 1
        entries = []
        cursor = nil
        loading = false
        failed = false
        searched = false
        searchedText = ""
    }

    func search(_ text: String) async {
        let text = text.trimmingCharacters(in: .whitespaces)
        guard !text.isEmpty else { return }
        reset()
        searchedText = text
        await request(append: false)
    }

    func loadMore() async {
        guard cursor != nil, !loading else { return }
        await request(append: true)
    }

    private func request(append: Bool) async {
        let gen = generation
        loading = true
        failed = false
        ConnectProgressManager.shared.startConnectProgress(
            NSLocalizedString("Searching directory…", comment: "in progress text"),
            owner: .directorySearch
        ) { [weak self] in
            Task { @MainActor in self?.reset() }
        }
        let r = await apiSearchDirectory(searchedText, cursor: cursor)
        ConnectProgressManager.shared.stopConnectProgress(.directorySearch)
        guard gen == generation else { return }
        loading = false
        searched = true
        guard let r else {
            failed = true
            return
        }
        cursor = r.cursor
        // the link is the identity of a result, so a row cannot appear twice across pages
        let known = Set(entries.map { $0.id })
        let fresh = r.entries.filter { !known.contains($0.id) }
        entries = append ? entries + fresh : fresh
    }
}

// Offered whenever there is search text, next to the connect-by-name row. Tapping it sends the
// text to the directory.
struct SearchInDirectoryRow: View {
    @EnvironmentObject var theme: AppTheme
    var searchText: String
    @FocusState.Binding var searchFocussed: Bool
    var onSearch: () -> Void

    var body: some View {
        HStack(spacing: 4) {
            Image(systemName: "magnifyingglass")
                .foregroundColor(theme.colors.primary)
            Text("Search in Directory")
                .foregroundColor(theme.colors.primary)
            Spacer()
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .contentShape(Rectangle())
        .onTapGesture {
            searchFocussed = false
            onSearch()
        }
    }
}

struct DirectorySearchRow: View {
    @EnvironmentObject var theme: AppTheme
    var entry: DirectorySearchEntry

    var body: some View {
        HStack(spacing: 8) {
            ProfileImage(imageStr: entry.image, size: 42)
            VStack(alignment: .leading, spacing: 2) {
                HStack(spacing: 4) {
                    Text(displayName).fontWeight(.bold).lineLimit(1)
                    if let simplexName = entry.simplexName {
                        // the directory's claim, shown as plain text: connecting uses the link
                        Text(simplexName)
                            .foregroundColor(theme.colors.secondary)
                            .lineLimit(1)
                    }
                }
                if let descr = entry.shortDescr, !descr.isEmpty {
                    Text(descr)
                        .foregroundColor(theme.colors.secondary)
                        .lineLimit(2)
                }
                Text(membersText)
                    .font(.caption)
                    .foregroundColor(theme.colors.secondary)
            }
            Spacer()
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .contentShape(Rectangle())
    }

    private var displayName: String { "#" + entry.displayName }

    private var membersText: String {
        String.localizedStringWithFormat(
            NSLocalizedString("%d members", comment: "directory search result"),
            Int(entry.entryType.summary.currentMembers)
        )
    }
}

// Shown before the first directory search of the session: the search text leaves the device,
// so the user is told before it does, not after.
func showDirectorySearchAlert(onSearch: @escaping () -> Void) {
    showAlert(
        NSLocalizedString("Search in Directory?", comment: "alert title"),
        message: NSLocalizedString("The text in the search field will be sent to SimpleX Directory to find public groups and channels.\n\nNo contact is created and your profile is not sent.", comment: "alert message"),
        actions: {[
            UIAlertAction(title: NSLocalizedString("Cancel", comment: "alert action"), style: .cancel),
            UIAlertAction(
                title: NSLocalizedString("Search", comment: "alert action"),
                style: .default,
                handler: { _ in onSearch() }
            ),
            UIAlertAction(
                title: NSLocalizedString("Search and don't show again", comment: "alert action"),
                style: .default,
                handler: { _ in
                    UserDefaults.standard.set(true, forKey: DEFAULT_DIRECTORY_SEARCH_ALERT_SHOWN)
                    onSearch()
                }
            )
        ]}
    )
}
