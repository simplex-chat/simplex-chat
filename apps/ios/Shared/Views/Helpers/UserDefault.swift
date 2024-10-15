//
//  UserDefault.swift
//  SimpleX (iOS)
//
//  Created by user on 14/10/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import Combine

@propertyWrapper
public struct UserDefault<Value: Equatable>: DynamicProperty {
    @StateObject private var observer = UserDefaultObserver()
    let initialValue: Value
    let key: String
    let store: UserDefaults

    public init(
        wrappedValue: Value,
        _ key: String,
        store: UserDefaults = .standard
    ) {
        self.initialValue = wrappedValue
        self.key = key
        self.store = store
    }

    public var wrappedValue: Value {
        get {
            // Observer can only be accessed after the property wrapper is installed in view (runtime exception)
            observer.subscribe(to: key)
            return store.object(forKey: key) as? Value ?? initialValue
        }
        nonmutating set {
            store.set(newValue, forKey: key)
        }
    }
}

private class UserDefaultObserver: ObservableObject {
    private var subscribed = false

    func subscribe(to key: String) {
        if !subscribed {
            NotificationCenter.default.addObserver(
                self,
                selector: #selector(userDefaultsDidChange),
                name: UserDefaults.didChangeNotification,
                object: nil
            )
            subscribed = true
        }
    }

    @objc
    private func userDefaultsDidChange(_ notification: Notification) {
        Task { @MainActor in objectWillChange.send() }
    }

    deinit { NotificationCenter.default.removeObserver(self) }
}
