//
//  FileProviderItem.swift
//  SimpleX Service
//
//  Created by Evgeny on 01/06/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import FileProvider
import UniformTypeIdentifiers
import SimpleXServiceProtocol

class FileProviderItem: NSObject, NSFileProviderItem {

    // TODO: implement an initializer to create an item from your extension's backing model
    // TODO: implement the accessors to return the values from your extension's backing model
    
    var itemIdentifier: NSFileProviderItemIdentifier { SERVICE_PROXY_ITEM_ID }
    
    var parentItemIdentifier: NSFileProviderItemIdentifier { NSFileProviderItemIdentifier("1") }
    
    var capabilities: NSFileProviderItemCapabilities {
        [.allowsReading, .allowsWriting, .allowsRenaming, .allowsReparenting, .allowsTrashing, .allowsDeleting]
    }
    
    var filename: String { "123" }
    
    var contentType: UTType {
        itemIdentifier == NSFileProviderItemIdentifier.rootContainer ? .folder : .plainText
    }

    var documentSize: NSNumber? { 1 }
}
