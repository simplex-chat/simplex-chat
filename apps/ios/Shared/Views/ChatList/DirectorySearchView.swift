//
//  DirectorySearchView.swift
//  SimpleX
//
//  Created by spaced4ndy on 13.08.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

// Directory results are not chats and are never saved: they live as long as the search text does.
@MainActor
class DirectorySearchModel: ObservableObject {
    @Published private(set) var entries: [DirectorySearchEntry] = []
    @Published private(set) var loading = false
    @Published private(set) var failed = false
    // tells "nothing searched yet" apart from "searched and found nothing"
    @Published private(set) var searched = false

    private var cursor: JSONValue? = nil
    private var searchedText = ""
    // bumped on every reset, so a reply that arrives too late cannot refill a list already cleared
    private var generation = 0

    var hasMore: Bool { cursor != nil }
    var showResults: Bool { loading || searched }

    func reset() {
        generation += 1
        entries = []
        cursor = nil
        loading = false
        failed = false
        searched = false
        searchedText = ""
        ConnectProgressManager.shared.stopConnectProgress(.directorySearch)
    }

    // a way out of the wait, not out of the results: the page already shown stays
    func cancelRequest() {
        generation += 1
        loading = false
    }

    func search(_ text: String) async {
        let text = text.trimmingCharacters(in: .whitespaces)
        guard !text.isEmpty, !(loading && text == searchedText) else { return }
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
            Task { @MainActor in self?.cancelRequest() }
        }
        let r = await apiSearchDirectory(searchedText, cursor: cursor)
        guard gen == generation else { return }
        ConnectProgressManager.shared.stopConnectProgress(.directorySearch)
        loading = false
        searched = true
        guard let r else {
            failed = true
            return
        }
        cursor = r.cursor
        let known = Set(entries.map { $0.id })
        let fresh = r.entries.filter { !known.contains($0.id) }
        entries = append ? entries + fresh : fresh
    }
}

struct SearchInDirectoryRow: View {
    @EnvironmentObject var theme: AppTheme
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
            ProfileImage(
                imageStr: entry.image,
                iconName: isChannel ? "antenna.radiowaves.left.and.right.circle.fill" : "person.2.circle.fill",
                size: 42
            )
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

    private var isChannel: Bool { entry.entryType.groupType == .channel }

    private var membersText: String {
        let summary = entry.entryType.summary
        let count = summary.publicMemberCount ?? summary.currentMembers
        return isChannel
            ? subscriberCountStr(count)
            : String.localizedStringWithFormat(NSLocalizedString("%d members", comment: "directory search result"), Int(count))
    }
}

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
