//
//  DirectorySearch.swift
//  SimpleX
//
//  Created by spaced4ndy on 12.08.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import Foundation
import SimpleXChat

// The directory's contact address, as published in docs/DIRECTORY.md. It must be the short
// link: only that form carries the address DR keys that service requests require, so the full
// links on the What's New cards cannot be substituted here.
let DIRECTORY_SERVICE_LINK = "https://smp4.simplex.im/a#lXUjJW5vHYQzoLYgmi8GbxkGP41_kjefFvBrdwg-0Ok"

// A service request is a full DR handshake, so it is slower than a local API call; the user
// gets a cancellable spinner while it runs and a retry row if it times out.
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

// entries stay as JSONValue so they can be decoded one by one: an entry the app cannot decode
// must not fail the whole response, as it would on a future directory field
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

// The response is a tagged object: searchResults or error. Anything else is a failure rather
// than something to parse leniently - it comes from outside the app.
func parseDirectorySearchResponse(_ resp: JSONValue) -> DirectorySearchResults? {
    guard let r: DirectorySearchResponse = decodeJSONValue(resp), r.type == "searchResults" else {
        return nil
    }
    let entries = (r.entries ?? []).compactMap { (e: JSONValue) -> DirectorySearchEntry? in
        decodeJSONValue(e)
    }
    return DirectorySearchResults(entries: entries, cursor: r.searchCursor)
}
