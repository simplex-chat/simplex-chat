import Foundation
import Security

protocol DatabasePassphraseStore: Sendable {
    func load() async throws -> String?
    func save(_ passphrase: String) async throws
    func delete() async throws
}

actor DatabasePassphraseKeychain: DatabasePassphraseStore {
    private let service: String
    private let account: String

    init(
        service: String = "chat.simplex.native.database",
        account: String = "simplex_v1"
    ) {
        self.service = service
        self.account = account
    }

    func load() async throws -> String? {
        var query = baseQuery()
        query[kSecReturnData] = true
        query[kSecMatchLimit] = kSecMatchLimitOne

        var result: CFTypeRef?
        let status = SecItemCopyMatching(query as CFDictionary, &result)
        switch status {
        case errSecSuccess:
            guard let data = result as? Data,
                  let passphrase = String(data: data, encoding: .utf8) else {
                throw DatabasePassphraseKeychainError.unexpectedData
            }
            return passphrase
        case errSecItemNotFound:
            return nil
        case errSecInteractionNotAllowed:
            throw DatabasePassphraseKeychainError.interactionNotAllowed
        default:
            throw DatabasePassphraseKeychainError.unhandledStatus(status)
        }
    }

    func save(_ passphrase: String) async throws {
        let data = Data(passphrase.utf8)
        let query = baseQuery()
        var addQuery = query
        addQuery[kSecValueData] = data
        addQuery[kSecAttrAccessible] = kSecAttrAccessibleWhenUnlockedThisDeviceOnly

        let addStatus = SecItemAdd(addQuery as CFDictionary, nil)
        switch addStatus {
        case errSecSuccess:
            return
        case errSecDuplicateItem:
            let updates: [CFString: Any] = [kSecValueData: data]
            let updateStatus = SecItemUpdate(query as CFDictionary, updates as CFDictionary)
            switch updateStatus {
            case errSecSuccess:
                return
            case errSecInteractionNotAllowed:
                throw DatabasePassphraseKeychainError.interactionNotAllowed
            default:
                throw DatabasePassphraseKeychainError.unhandledStatus(updateStatus)
            }
        case errSecInteractionNotAllowed:
            throw DatabasePassphraseKeychainError.interactionNotAllowed
        default:
            throw DatabasePassphraseKeychainError.unhandledStatus(addStatus)
        }
    }

    func delete() async throws {
        let status = SecItemDelete(baseQuery() as CFDictionary)
        switch status {
        case errSecSuccess, errSecItemNotFound:
            return
        case errSecInteractionNotAllowed:
            throw DatabasePassphraseKeychainError.interactionNotAllowed
        default:
            throw DatabasePassphraseKeychainError.unhandledStatus(status)
        }
    }

    private func baseQuery() -> [CFString: Any] {
        [
            kSecClass: kSecClassGenericPassword,
            kSecAttrService: service,
            kSecAttrAccount: account,
            kSecUseDataProtectionKeychain: true,
        ]
    }
}

enum DatabasePassphraseKeychainError: LocalizedError, Equatable, Sendable {
    case interactionNotAllowed
    case unexpectedData
    case unhandledStatus(OSStatus)

    var errorDescription: String? {
        switch self {
        case .interactionNotAllowed:
            "The Mac Keychain is locked. Unlock your Mac and try again."
        case .unexpectedData:
            "The saved database passphrase could not be read."
        case let .unhandledStatus(status):
            SecCopyErrorMessageString(status, nil) as String?
                ?? "The Mac Keychain returned error \(status)."
        }
    }
}
