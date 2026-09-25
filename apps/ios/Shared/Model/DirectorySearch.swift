//
//  DirectorySearch.swift
//  SimpleX
//
//  Created by spaced4ndy on 12.08.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import Foundation
import SimpleXChat

// The directory's address, as published in docs/DIRECTORY.md. It must be the short link form -
// only that one carries the keys a service request needs; a full link fails every time.
let DIRECTORY_SERVICE_LINK = "https://smp4.simplex.im/a#lXUjJW5vHYQzoLYgmi8GbxkGP41_kjefFvBrdwg-0Ok"

// A search sets up an encrypted connection, so it takes seconds, not milliseconds.
let DIRECTORY_SEARCH_TIMEOUT_SEC: Double = 10

struct DirectoryPublicLink: Decodable, Hashable {
    var connFullLink: String? = nil
    var connShortLink: String? = nil
}

struct DirectoryEntryType: Decodable, Hashable {
    var groupType: GroupType? = nil
    var summary: GroupSummary
}

struct DirectorySearchEntry: Decodable, Hashable, Identifiable {
    var entryType: DirectoryEntryType
    var displayName: String
    var simplexName: String? = nil
    var groupLink: DirectoryPublicLink
    var shortDescr: String? = nil
    var image: String? = nil
    var activeAt: Date? = nil
    var createdAt: Date? = nil

    // the directory drops entries with no link, but the response is untrusted input
    var connectLink: String? { groupLink.connShortLink ?? groupLink.connFullLink }

    // the link is stable and unique per entry, so it also de-duplicates across pages
    var id: String { connectLink ?? displayName }
}

// entries stay undecoded here so they can be decoded one at a time: one entry the app does not
// understand, because a newer directory added a field, must not discard the whole response
private struct DirectorySearchResponse: Decodable {
    var type: String
    var entries: [JSONValue]?
    var searchCursor: JSONValue?
}

struct DirectorySearchResults {
    var entries: [DirectorySearchEntry]
    // opaque: stored and echoed back on the next request, never inspected
    var cursor: JSONValue?
}

func directorySearchRequestJSON(_ text: String, _ cursor: JSONValue?) -> String {
    var req: [String: JSONValue] = ["type": .string("search"), "searchText": .string(text)]
    if let cursor { req["searchCursor"] = cursor }
    return encodeJSON(JSONValue.object(req))
}

// Anything but a well-formed results response is a failure, not something to salvage: it comes
// from another party, not from our own core.
func parseDirectorySearchResponse(_ resp: JSONValue) -> DirectorySearchResults? {
    guard let r: DirectorySearchResponse = decodeJSONValue(resp), r.type == "searchResults" else {
        return nil
    }
    let entries = (r.entries ?? []).compactMap { (e: JSONValue) -> DirectorySearchEntry? in
        decodeJSONValue(e)
    }
    return DirectorySearchResults(entries: entries, cursor: r.searchCursor)
}
