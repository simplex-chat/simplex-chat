import AppKit
import CoreBridge
import Darwin
import Foundation

final class SingleInstanceGuard {
    private let descriptor: Int32

    init?(lockURL: URL = SingleInstanceGuard.defaultLockURL) {
        do {
            try FileManager.default.createDirectory(
                at: lockURL.deletingLastPathComponent(),
                withIntermediateDirectories: true
            )
        } catch {
            return nil
        }

        let descriptor = Darwin.open(lockURL.path, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR)
        guard descriptor >= 0 else { return nil }
        guard sx_try_lock_file(descriptor) else {
            Darwin.close(descriptor)
            return nil
        }
        self.descriptor = descriptor
    }

    deinit {
        sx_unlock_file(descriptor)
        Darwin.close(descriptor)
    }

    static var defaultLockURL: URL {
        FileManager.default.homeDirectoryForCurrentUser
            .appendingPathComponent(".local/share/simplex", isDirectory: true)
            .appendingPathComponent("simplex-native.lock")
    }

    @MainActor
    static func activateExistingApplication() {
        guard let bundleIdentifier = Bundle.main.bundleIdentifier else { return }
        let currentProcessID = ProcessInfo.processInfo.processIdentifier
        NSRunningApplication.runningApplications(withBundleIdentifier: bundleIdentifier)
            .first(where: { $0.processIdentifier != currentProcessID })?
            .activate(options: [.activateAllWindows])
    }
}
