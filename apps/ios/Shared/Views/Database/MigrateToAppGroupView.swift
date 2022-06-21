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
        ZStack(alignment: .topLeading) {
            Text("Database migration").font(.largeTitle)

            switch v3DBMigration {
            case .offer:
                VStack(alignment: .leading, spacing: 16) {
                    Text("To support instant push notifications we have to relocate the database.")
                    Text("If you need to use the chat, you can do it later by restarting the app.")
                }
                .padding(.top, 56)
                center {
                    Button {
                        migrateDatabaseToV3()
                    } label: {
                        Text("Start migration")
                            .font(.title)
                            .frame(maxWidth: .infinity)
                    }
                }
                skipMigration()
            case .exporting:
                center {
                    ProgressView(value: 0.33)
                    Text("Exporting database archive...")
                }
            case .export_error:
                center {
                    Text("Export error:").font(.headline)
                    Text(migrationError)
                    Text("Please make the screenshot and send it to the developers to [chat@simplex.chat](mailto:chat@simplex.chat) or via chat.")
                }
                skipMigration()
            case .exported:
                center {
                    Text("Exported database archive.")
                }
            case .migrating:
                center {
                    ProgressView(value: 0.67)
                    Text("Migrating database archive...")
                }
            case .migration_error:
                center {
                    Text("Migration error:").font(.headline)
                    Text(migrationError)
                }
                skipMigration()
            case .migrated:
                center {
                    ProgressView(value: 1.0)
                    Text("Migration is completed")
                }
                VStack {
                    Spacer()
                    Spacer()
                    Spacer()
                    Button {
                        setV3DBMigration(.ready)
                        dbContainerGroupDefault.set(.group)
                        startChat()
                    } label: {
                        Text("Start using chat")
                            .font(.title)
                            .frame(maxWidth: .infinity)
                    }
                    Spacer()
                }
                .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .bottom)
            default:
                Spacer()
                Text("Unexpected migration state")
                Text("\(v3DBMigration.rawValue)")
                Spacer()
                skipMigration()
            }
        }
        .padding()
    }

    private func center<Content>(@ViewBuilder c: @escaping () -> Content) -> some View where Content: View {
        VStack(alignment: .leading, spacing: 8) { c() }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .leading)
    }

    private func skipMigration() -> some View {
        ZStack {
            Button {
                setV3DBMigration(.postponed)
                startChat()
            } label: {
                Text("Skip and start using chat")
                    .frame(maxWidth: .infinity, alignment: .trailing)
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .bottomTrailing)
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
                try! await Task.sleep(nanoseconds: 2_000_000_000)
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
            do {
                await MainActor.run { setV3DBMigration(.migrating) }
                try! await Task.sleep(nanoseconds: 2_000_000_000)
//                try await apiImportArchive(config: config)
                await MainActor.run { setV3DBMigration(.migrated) }
            } catch let error {
                await MainActor.run { setV3DBMigration(.migration_error) }
                migrationError = responseError(error)
            }
        }
    }
}


struct MigrateToGroupView_Previews: PreviewProvider {
    static var previews: some View {
        MigrateToAppGroupView()
    }
}
