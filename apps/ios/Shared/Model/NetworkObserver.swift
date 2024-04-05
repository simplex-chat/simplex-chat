//
//  NetworkObserver.swift
//  SimpleX (iOS)
//
//  Created by Avently on 05.04.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import Foundation
import Network
import SimpleXChat

class NetworkObserver {
    let onChange: (NetworkInfo) -> Void
    private var prevInfo: NetworkInfo? = nil
    private let queue: DispatchQueue = DispatchQueue(label: "NetworkObserver")

    init(onChange: @escaping (NetworkInfo) -> Void) {
        self.onChange = onChange
    }

    private let monitor: NWPathMonitor = NWPathMonitor()

    private func start() {
        monitor.pathUpdateHandler = { [weak self] path in
            self?.networkPathChanged(path: path)
        }
        monitor.start(queue: queue)
    }

    private func stop() {
        monitor.cancel()
    }

    private func networkPathChanged(path: NWPath) {
        let info = NetworkInfo(
            online: path.status == .satisfied,
            type: networkTypeFromPath(path: path),
            metered: path.isExpensive
        )
        if (prevInfo != info) {
            prevInfo = info
            onChange(info)
        }
    }

    private func networkTypeFromPath(path: NWPath) -> NetworkInfoType {
        return if path.usesInterfaceType(.cellular) {
            .cellular
        } else if path.usesInterfaceType(.wifi) {
            .wifi
        } else if path.usesInterfaceType(.wiredEthernet) {
            .ethernet
        } else {
            .other
        }
    }

    private static var networkObserver: NetworkObserver? = nil

    static func reinitNetworkObserver() {
        networkObserver?.stop()
        // When having both mobile and Wi-Fi networks enabled with Wi-Fi being active, then disabling Wi-Fi, network reports its offline (which is true)
        // but since it will be online after switching to mobile, there is no need to inform backend about such temporary change.
        // But if it will not be online after some seconds, report it and apply required measures
        var noNetworkTask: Task = Task {}
        let observer = NetworkObserver { info in
            logger.debug("Network changed: \(String(describing: info))")
            noNetworkTask.cancel()
            if (info.online) {
                if hasChatCtrl() {
                    _ = apiSetNetworkInfo(info)
                }
            } else {
                noNetworkTask = Task {
                    do {
                        try await Task.sleep(nanoseconds: 3000_000000)
                        if hasChatCtrl() {
                            _ = apiSetNetworkInfo(info)
                        }
                    } catch {
                        logger.debug("Interrupted sleep before sending network info")
                    }
                }
            }
        }
        observer.start()
        networkObserver = observer
    }
}
