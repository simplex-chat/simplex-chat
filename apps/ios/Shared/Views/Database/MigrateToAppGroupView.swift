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
    @State private var migrationError = ""
    @AppStorage(DEFAULT_CHAT_ARCHIVE_NAME) private var chatArchiveName: String?
    @AppStorage(DEFAULT_CHAT_ARCHIVE_TIME) private var chatArchiveTime: Double = 0

    var body: some View {
        ZStack(alignment: .topLeading) {
            Text("Push notifications").font(.largeTitle)

            switch chatModel.v3DBMigration {
            case .offer:
                VStack(alignment: .leading, spacing: 16) {
                    Text("To support instant push notifications the chat database has to be migrated.")
                    Text("If you need to use the chat now tap **Do it later** below (you will be offered to migrate the database when you restart the app).")
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
                    Text("Exporting database archive…")
                }
                migrationProgress()
            case .export_error:
                migrationFailed().padding(.top, 56)
                center {
                    Text("Export error:").font(.headline)
                    Text(migrationError)
                }
                skipMigration()
            case .exported:
                center {
                    Text("Exported database archive.")
                }
            case .migrating:
                center {
                    ProgressView(value: 0.67)
                    Text("Migrating database archive…")
                }
                migrationProgress()
            case .migration_error:
                VStack(alignment: .leading, spacing: 16) {
                    migrationFailed()
                    Text("The created archive is available via app Settings / Database / Old database archive.")
                }
                .padding(.top, 56)
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
                        do {
                            resetChatCtrl()
                            try initializeChat(start: true)
                            onboardingStageDefault.set(.step4_SetNotificationsMode)
                            chatModel.onboardingStage = .step4_SetNotificationsMode
                            setV3DBMigration(.ready)
                        } catch let error {
                            dbContainerGroupDefault.set(.documents)
                            setV3DBMigration(.migration_error)
                            migrationError = "Error starting chat: \(responseError(error))"
                        }
                        deleteOldArchive()
                    } label: {
                        Text("Start chat")
                            .font(.title)
                            .frame(maxWidth: .infinity)
                    }
                    Spacer()
                }
                .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .bottom)
            default:
                Spacer()
                Text("Unexpected migration state")
                Text("\(chatModel.v3DBMigration.rawValue)")
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

    private func migrationProgress() -> some View {
        VStack {
            Spacer()
            ProgressView().scaleEffect(2)
            Spacer()
            Spacer()
            Spacer()
        }
        .frame(maxWidth: .infinity)
    }

    private func migrationFailed() -> some View {
        Text("Migration failed. Tap **Skip** below to continue using the current database. Please report the issue to the app developers via chat or email [chat@simplex.chat](mailto:chat@simplex.chat).")
    }

    private func skipMigration() -> some View {
        ZStack {
            Button {
                setV3DBMigration(.postponed)
                do {
                    try startChat()
                } catch let error {
                    fatalError("Failed to start or load chats: \(responseError(error))")
                }
            } label: {
                Text("Do it later")
                    .frame(maxWidth: .infinity, alignment: .trailing)
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .bottomTrailing)
    }

    private func setV3DBMigration(_ state: V3DBMigrationState) {
        chatModel.v3DBMigration = state
        v3DBMigrationDefault.set(state)
    }

    func migrateDatabaseToV3() {
        setV3DBMigration(.exporting)
        let archiveTime = Date.now
        let archiveName = "simplex-chat.\(archiveTime.ISO8601Format()).zip"
        chatArchiveTime = archiveTime.timeIntervalSince1970
        chatArchiveName = archiveName
        let config = ArchiveConfig(archivePath: getDocumentsDirectory().appendingPathComponent(archiveName).path)
        Task {
            do {
                try apiSaveAppSettings(settings: AppSettings.current.prepareForExport())
                try? FileManager.default.createDirectory(at: getWallpaperDirectory(), withIntermediateDirectories: true)
                _ = try await apiExportArchive(config: config)
                await MainActor.run { setV3DBMigration(.exported) }
            } catch let error {
                await MainActor.run {
                    setV3DBMigration(.export_error)
                    migrationError = responseError(error)
                }
                return
            }

            do {
                await MainActor.run { setV3DBMigration(.migrating) }
                dbContainerGroupDefault.set(.group)
                resetChatCtrl()
                try await MainActor.run { try initializeChat(start: false) }
                let _ = try await apiImportArchive(config: config)
                let appSettings = try apiGetAppSettings(settings: AppSettings.current.prepareForExport())
                await MainActor.run {
                    appSettings.importIntoApp()
                    setV3DBMigration(.migrated)
                }
            } catch let error {
                dbContainerGroupDefault.set(.documents)
                await MainActor.run {
                    setV3DBMigration(.migration_error)
                    migrationError = responseError(error)
                }
            }
        }
    }
}

func exportChatArchive(_ storagePath: URL? = nil) async throws -> (URL, [ArchiveError]) {
    let archiveTime = Date.now
    let ts = archiveTime.ISO8601Format(Date.ISO8601FormatStyle(timeSeparator: .omitted))
    let archiveName = "simplex-chat.\(ts).zip"
    let archivePath = (storagePath ?? getDocumentsDirectory()).appendingPathComponent(archiveName)
    let config = ArchiveConfig(archivePath: archivePath.path)
    // Settings should be saved before changing a passphrase, otherwise the database needs to be migrated first
    if !ChatModel.shared.chatDbChanged {
        try apiSaveAppSettings(settings: AppSettings.current.prepareForExport())
    }
    try? FileManager.default.createDirectory(at: getWallpaperDirectory(), withIntermediateDirectories: true)
    let errs = try await apiExportArchive(config: config)
    if storagePath == nil {
        deleteOldArchive()
        UserDefaults.standard.set(archiveName, forKey: DEFAULT_CHAT_ARCHIVE_NAME)
        chatArchiveTimeDefault.set(archiveTime)
    }
    return (archivePath, errs)
}

func deleteOldArchive() {
    let d = UserDefaults.standard
    if let archiveName = d.string(forKey: DEFAULT_CHAT_ARCHIVE_NAME) {
        do {
            try FileManager.default.removeItem(atPath: getDocumentsDirectory().appendingPathComponent(archiveName).path)
            d.set(nil, forKey: DEFAULT_CHAT_ARCHIVE_NAME)
            d.set(0, forKey: DEFAULT_CHAT_ARCHIVE_TIME)
        } catch let error {
            logger.error("removeItem error \(String(describing: error))")
        }
    }
}

struct MigrateToGroupView_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.v3DBMigration = .migrated
        return MigrateToAppGroupView()
            .environmentObject(chatModel)
    }
}
