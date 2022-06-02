//
//  FileProviderExtension.swift
//  SimpleX Service
//
//  Created by Evgeny on 01/06/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import FileProvider
import OSLog
import SimpleXChat

let logger = Logger()
let serviceListener = NSXPCListener.service()
let listenerDelegate = SimpleXFPServiceDelegate()
var machMessenger = MachMessenger(FPS_MACH_PORT, callback: receivedAppMachMessage)

func receivedAppMachMessage(_ msgId: Int32, msg: String) -> String? {
    logger.debug("MachMessenger: FileProviderExtension receivedAppMachMessage \"\(msg)\" from App, replying")
    return "reply from FPS to: \(msg)"
}

class FileProviderExtension: NSFileProviderExtension {
    var fileManager = FileManager()

    override init() {
        logger.debug("FileProviderExtension.init")
        super.init()
        machMessenger.start()
        serviceListener.delegate = listenerDelegate

        do {
            let endPointData = try NSKeyedArchiver.archivedData(withRootObject: serviceListener.endpoint, requiringSecureCoding: true)
            let err = machMessenger.sendMessage(APP_MACH_PORT, data: endPointData)
            logger.debug("FileProviderExtension.MachMessenger.sendMessage with endpoint res \(String(describing: err), privacy: .public)")
    //        let res = machMessenger.sendMessageWithReply(APP_MACH_PORT, msg: "machMessenger in FileProviderExtension")
    //        logger.debug("FileProviderExtension MachMessenger app reply \(String(describing: res), privacy: .public)")
        } catch let err {
            logger.debug("FileProviderExtension.MachMessenger.sendMessage error \(String(describing: err), privacy: .public)")
        }


        let manager = NSFileProviderManager.default
        logger.debug("FileProviderExtension.init NSFileProviderManager \(manager.documentStorageURL, privacy: .public)")

//        FileManager.default.createFile(atPath: "\(manager.documentStorageURL)123", contents: "hello".data(using: .utf8))

        self.providePlaceholder(at: URL(string: "\(manager.documentStorageURL)123")!) { err in
            if let err = err {
                logger.debug("FileProviderExtension.providePlaceholder error \(String(describing: err), privacy: .public)")
            } else {
                logger.debug("FileProviderExtension.providePlaceholder ok") // <-- this returns ok
//                self.startProvidingItem(at: URL(string: "\(manager.documentStorageURL)123")!) { err in
//                    if let err = err {
//                        logger.debug("FileProviderExtension.startProvidingItem error \(String(describing: err), privacy: .public)")
//                    } else {
//                        logger.debug("FileProviderExtension.startProvidingItem ok")
//                    }
//                }
            }
        }

        Task { serviceListener.resume() }
    }
    
    override func item(for identifier: NSFileProviderItemIdentifier) throws -> NSFileProviderItem {
        logger.debug("FileProviderExtension.item")
        // resolve the given identifier to a record in the model
        
        // TODO: implement the actual lookup
        return FileProviderItem()
    }
    
    override func urlForItem(withPersistentIdentifier identifier: NSFileProviderItemIdentifier) -> URL? {
        logger.debug("FileProviderExtension.urlForItem")
        // resolve the given identifier to a file on disk
        guard let item = try? item(for: identifier) else {
            return nil
        }
        
        // in this implementation, all paths are structured as <base storage directory>/<item identifier>/<item file name>
        let manager = NSFileProviderManager.default
        let perItemDirectory = manager.documentStorageURL.appendingPathComponent(identifier.rawValue, isDirectory: true)

        logger.debug("FileProviderExtension.urlForItem NSFileProviderManager \(manager.documentStorageURL, privacy: .public)")

        return perItemDirectory.appendingPathComponent(item.filename, isDirectory:false)
    }

    func identifierForItemAtURL(_ url: URL, completionHandler: @escaping (NSFileProviderItemIdentifier?) -> Void) {
        completionHandler(SERVICE_PROXY_ITEM_ID)
    }

    override func persistentIdentifierForItem(at url: URL) -> NSFileProviderItemIdentifier? {
        logger.debug("FileProviderExtension.persistentIdentifierForItem")
//        if url == SERVICE_PROXY_ITEM_URL { return SERVICE_PROXY_ITEM_ID }
        return SERVICE_PROXY_ITEM_ID

        // resolve the given URL to a persistent identifier using a database
        let pathComponents = url.pathComponents
        
        // exploit the fact that the path structure has been defined as
        // <base storage directory>/<item identifier>/<item file name> above
        assert(pathComponents.count > 2)
        
        return NSFileProviderItemIdentifier(pathComponents[pathComponents.count - 2])
    }
    
    override func providePlaceholder(at url: URL, completionHandler: @escaping (Error?) -> Void) {
        logger.debug("FileProviderExtension.providePlaceholder")
        guard let identifier = persistentIdentifierForItem(at: url) else {
            completionHandler(NSFileProviderError(.noSuchItem))
            return
        }

        do {
            let fileProviderItem = try item(for: identifier)
            let placeholderURL = NSFileProviderManager.placeholderURL(for: url)
            try NSFileProviderManager.writePlaceholder(at: placeholderURL, withMetadata: fileProviderItem)
            completionHandler(nil)
        } catch let error {
            completionHandler(error)
        }
    }

    override func startProvidingItem(at url: URL, completionHandler: @escaping ((_ error: Error?) -> Void)) {
        logger.debug("FileProviderExtension.startProvidingItem")
        completionHandler(nil)
//        if url == SERVICE_PROXY_ITEM_URL {
//            completionHandler(nil)
//            return
//        }

        // Should ensure that the actual file is in the position returned by URLForItemWithIdentifier:, then call the completion handler
        
        /* TODO:
         This is one of the main entry points of the file provider. We need to check whether the file already exists on disk,
         whether we know of a more recent version of the file, and implement a policy for these cases. Pseudocode:
         
         if !fileOnDisk {
             downloadRemoteFile()
             callCompletion(downloadErrorOrNil)
         } else if fileIsCurrent {
             callCompletion(nil)
         } else {
             if localFileHasChanges {
                 // in this case, a version of the file is on disk, but we know of a more recent version
                 // we need to implement a strategy to resolve this conflict
                 moveLocalFileAside()
                 scheduleUploadOfLocalFile()
                 downloadRemoteFile()
                 callCompletion(downloadErrorOrNil)
             } else {
                 downloadRemoteFile()
                 callCompletion(downloadErrorOrNil)
             }
         }
         */
        
        completionHandler(NSError(domain: NSCocoaErrorDomain, code: NSFeatureUnsupportedError, userInfo:[:]))
    }
    
    
    override func itemChanged(at url: URL) {
        logger.debug("FileProviderExtension.itemChanged")
        // Called at some point after the file has changed; the provider may then trigger an upload
        
        /* TODO:
         - mark file at <url> as needing an update in the model
         - if there are existing NSURLSessionTasks uploading this file, cancel them
         - create a fresh background NSURLSessionTask and schedule it to upload the current modifications
         - register the NSURLSessionTask with NSFileProviderManager to provide progress updates
         */
    }
    
    override func stopProvidingItem(at url: URL) {
        logger.debug("FileProviderExtension.stopProvidingItem")
        // Called after the last claim to the file has been released. At this point, it is safe for the file provider to remove the content file.
        // Care should be taken that the corresponding placeholder file stays behind after the content file has been deleted.
        
        // Called after the last claim to the file has been released. At this point, it is safe for the file provider to remove the content file.
        
        // TODO: look up whether the file has local changes
        let fileHasLocalChanges = false
        
        if !fileHasLocalChanges {
            // remove the existing file to free up space
            do {
                _ = try FileManager.default.removeItem(at: url)
            } catch {
                // Handle error
            }
            
            // write out a placeholder to facilitate future property lookups
            self.providePlaceholder(at: url, completionHandler: { error in
                // TODO: handle any error, do any necessary cleanup
            })
        }
    }
    
    // MARK: - Actions
    
    /* TODO: implement the actions for items here
     each of the actions follows the same pattern:
     - make a note of the change in the local model
     - schedule a server request as a background task to inform the server of the change
     - call the completion block with the modified item in its post-modification state
     */
    
    // MARK: - Enumeration
    
    override func enumerator(for containerItemIdentifier: NSFileProviderItemIdentifier) throws -> NSFileProviderEnumerator {
        logger.debug("FileProviderExtension.enumerator")

        let maybeEnumerator: NSFileProviderEnumerator? = nil
        if (containerItemIdentifier == NSFileProviderItemIdentifier.rootContainer) {
            // TODO: instantiate an enumerator for the container root
        } else if (containerItemIdentifier == NSFileProviderItemIdentifier.workingSet) {
            // TODO: instantiate an enumerator for the working set
        } else {
            // TODO: determine if the item is a directory or a file
            // - for a directory, instantiate an enumerator of its subitems
            // - for a file, instantiate an enumerator that observes changes to the file
        }
        guard let enumerator = maybeEnumerator else {
            throw NSError(domain: NSCocoaErrorDomain, code: NSFeatureUnsupportedError, userInfo:[:])
        }
        return enumerator
    }
}
