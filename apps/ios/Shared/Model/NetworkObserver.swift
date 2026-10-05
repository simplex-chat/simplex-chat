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
    static let shared = NetworkObserver()
    private let queue: DispatchQueue = DispatchQueue(label: "chat.simplex.app.NetworkObserver")
    private var prevInfo: UserNetworkInfo? = nil
    // The core reconnects to the servers on every online network event, so the event is also sent when
    // the network type is the same, but the interface changed or an address or a gateway disappeared -
    // the connections via them are dead (added ones, e.g. IPv6 after IPv4, do not affect them).
    private var prevInterface: String? = nil
    private var prevGateways: Set<String> = []
    private var prevAddresses: Set<String> = []
    private var monitor: NWPathMonitor?
    private let monitorLock: DispatchQueue = DispatchQueue(label: "chat.simplex.app.monitorLock")

    func restartMonitor() {
        monitorLock.sync {
            monitor?.cancel()
            // the chat controller is created anew, so the first path update is reported to it;
            // it is reset after the update being handled on the monitor queue, so that one that
            // reported to the previous controller does not suppress the report to the new one
            queue.async { [weak self] in self?.prevInfo = nil }
            let mon = NWPathMonitor()
            mon.pathUpdateHandler = { [weak self] path in
                self?.networkPathChanged(path: path)
            }
            mon.start(queue: queue)
            monitor = mon
        }
    }

    private func networkPathChanged(path: NWPath) {
        let info = UserNetworkInfo(
            networkType: networkTypeFromPath(path),
            online: path.status == .satisfied
        )
        let gateways = Set(path.gateways.map { "\($0)" })
        // an interface of a type the path uses, not the most preferred available one - Wi-Fi can stay associated
        // without internet while the path is satisfied via cellular, and then its addresses are compared
        let interface = path.availableInterfaces.first { path.usesInterfaceType($0.type) }?.name
        let addresses = localAddresses(interface)
        // the interface is compared too - a VPN that reconnected as another one can have the same
        // address, and an interface being configured has none yet
        let pathChanged = interface != prevInterface || lost(prevGateways, gateways) || lost(prevAddresses, addresses)
        // the report that was not accepted is repeated when the next path update is received
        if prevInfo != info || (info.online && pathChanged) {
            prevInfo = setNetworkInfo(info) ? info : nil
        }
        // the path is recorded when it is not reported too, so that what appeared and then disappeared
        // is not compared with the state before it appeared
        prevInterface = interface
        prevGateways = gateways
        prevAddresses = addresses
    }

    // one that disappeared means the connections via it are dead, and one that appeared when there were
    // none means the network can be used again; added ones (e.g. IPv6 after IPv4) do not affect connections
    private func lost(_ prev: Set<String>, _ curr: Set<String>) -> Bool {
        !curr.isSuperset(of: prev) || (prev.isEmpty && !curr.isEmpty)
    }

    // NWPath has no addresses, and a changed address with the same interface and gateway
    // (e.g. the lease was renewed after internet loss) also means the connections via it are dead
    private func localAddresses(_ interface: String?) -> Set<String> {
        var ifap: UnsafeMutablePointer<ifaddrs>?
        guard let interface = interface, getifaddrs(&ifap) == 0, let first = ifap else { return [] }
        defer { freeifaddrs(ifap) }
        return Set(sequence(first: first, next: { $0.pointee.ifa_next }).compactMap { ifa -> String? in
            guard let sa = ifa.pointee.ifa_addr,
                  sa.pointee.sa_family == UInt8(AF_INET) || sa.pointee.sa_family == UInt8(AF_INET6),
                  String(cString: ifa.pointee.ifa_name) == interface else { return nil }
            var host = [CChar](repeating: 0, count: Int(NI_MAXHOST))
            guard getnameinfo(sa, socklen_t(sa.pointee.sa_len), &host, socklen_t(host.count), nil, 0, NI_NUMERICHOST) == 0 else { return nil }
            return String(cString: host)
        })
    }

    private func networkTypeFromPath(_ path: NWPath) -> UserNetworkType {
        if path.usesInterfaceType(.wiredEthernet) {
            .ethernet
        } else if path.usesInterfaceType(.wifi) {
            .wifi
        } else if path.usesInterfaceType(.cellular) {
            .cellular
        } else if path.usesInterfaceType(.other) {
            .other
        } else {
            .none
        }
    }

    private static var networkObserver: NetworkObserver? = nil

    private func setNetworkInfo(_ info: UserNetworkInfo) -> Bool {
        logger.debug("setNetworkInfo Network changed: \(String(describing: info))")
        DispatchQueue.main.sync {
            ChatModel.shared.networkInfo = info
        }
        return self.monitorLock.sync { () -> Bool in
            guard let ctrl = currentChatCtrl() else { return false }
            do {
                // the controller is passed, so that resetting it while the report is sent is not a crash
                try apiSetNetworkInfo(info, ctrl: ctrl)
                return true
            } catch let err {
                logger.error("setNetworkInfo error: \(responseError(err))")
                return false
            }
        }
    }
}
