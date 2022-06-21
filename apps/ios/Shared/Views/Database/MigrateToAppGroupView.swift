//
//  MigrateToGroupView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 20/06/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

enum V3DBMigrationState: String {
    case offer
    case postponed
    case exporting
    case export_error
    case exported
    case migrating
    case migration_error
    case migrated
    case ready

    var startChat: Bool {
        switch self {
        case .postponed: return true
        case .ready: return true
        default: return false
        }
    }
}

let v3DBMigrationDefault = EnumDefault<V3DBMigrationState>(
    defaults: UserDefaults.standard,
    forKey: DEFAULT_CHAT_V3_DB_MIGRATION,
    withDefault: .offer
)

struct MigrateToAppGroupView: View {
    @EnvironmentObject var chatModel: ChatModel
    @State private var v3DBMigration = v3DBMigrationDefault.get()
    @State private var migrationError = ""
    @AppStorage(DEFAULT_CHAT_ARCHIVE_PATH) private var chatArchivePath: String?
    @AppStorage(DEFAULT_CHAT_ARCHIVE_TIME) private var chatArchiveTime: Double = 0

    var body: some View {
        VStack {
            Text("Database migration").font(.title)

            switch v3DBMigration {
            case .offer:
                offerMigration()
            case .exporting:
                Text("Exporting database archive...")
                ProgressView(value: 0.33)
            case .export_error:
                Text("Export error.")
                skipMigration()
            case .exported:
                Text("Exported database archive.")
            case .migrating:
                Text("Migrating database archive...")
                ProgressView(value: 0.67)
            case .migration_error:
                Text("Migration error.")
            case .migrated:
                Text("Database is migrated.")
                ProgressView(value: 1.0)
            default:
                Text("\(v3DBMigration.rawValue)")
            }
        }
    }

    func offerMigration() -> some View {
        VStack {
            Text("To support instant push notifications we have to relocate the database.")
            Text("You can do it later by restarting the app.")
            skipMigration()
            Button {
                migrateDatabaseToV3()
            } label: {
                Text("Start migration")
            }
        }
    }

    func skipMigration() -> some View {
        Button {
            setV3DBMigration(.postponed)
            startChat()
        } label: {
            Text("Skip and open chats")
        }
    }

    private func setV3DBMigration(_ value: V3DBMigrationState) {
        v3DBMigration = value
        v3DBMigrationDefault.set(value)
    }

    func migrateDatabaseToV3() {
        v3DBMigration = .exporting
        let archiveTime = Date.now
        let archivePath = getDocumentsDirectory().appendingPathComponent("simplex-chat.\(archiveTime.ISO8601Format()).zip").path
        chatArchiveTime = archiveTime.timeIntervalSince1970
        chatArchivePath = archivePath
        let config = ArchiveConfig(archivePath: archivePath)
        Task {
            do {
                try! await Task.sleep(nanoseconds: 1_000_000_000)
                try await apiExportArchive(config: config)
                await MainActor.run { setV3DBMigration(.exported) }
            } catch let error {
                await MainActor.run { setV3DBMigration(.export_error) }
                migrationError = responseError(error)
                return
            }

// TODO:
// 1. initialize the new chat controller here that would use the database in the new location
// 2. import db
// 3. re-initialize chat controller
//            do {
//                await MainActor.run { setV3DBMigration(.migrating) }
//                try await apiImportArchive(config: config)
//                await MainActor.run { setV3DBMigration(.migrated) }
//            } catch let error {
//                await MainActor.run { setV3DBMigration(.migration_error) }
//                migrationError = responseError(error)
//            }
        }
    }
}


struct MigrateToGroupView_Previews: PreviewProvider {
    static var previews: some View {
        MigrateToAppGroupView()
    }
}
