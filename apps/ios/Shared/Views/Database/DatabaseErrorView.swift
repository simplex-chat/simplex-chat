//
//  DatabaseErrorView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 04/09/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct DatabaseErrorView: View {
    @EnvironmentObject var m: ChatModel
    var status: DBMigrationResult
    @State private var dbKey = ""

    var body: some View {
        VStack(alignment: .leading, spacing: 16) {
            switch status {
            case let .errorNotADatabase(dbFile):
                Text("Wrong database password")
                    .font(.title)
                Text("Database password is different from saved in the keychain, or database file is invalid")
                TextField("Enter database password", text: $dbKey)
                    .disableAutocorrection(true)
                    .autocapitalization(.none)
                Button("Save password") {
                    _ = setDatabaseKey(dbKey)
                    do {
                        try initializeChat(start: m.v3DBMigration.startChat)
                    } catch let error {
                        logger.error("initializeChat \(responseError(error))")
                    }
                }
                Spacer()
                Text("File: \(dbFile)")
            case let .error(dbFile, migrationError):
                Text("Database error")
                    .font(.title)
                Text("File: \(dbFile)")
                Text("Error: \(migrationError)")
                Spacer()
            case .errorKeychain:
                Text("Keychain error")
                    .font(.title)
                Text("Cannot access keychain to save database password")
                Spacer()
            case let .unknown(json):
                Text("Database error")
                    .font(.title)
                Text("Unknown database error: \(json)")
                Spacer()
            case .ok:
                EmptyView()
            }
        }
        .padding()
        .frame(maxHeight: .infinity)    }
}

struct DatabaseErrorView_Previews: PreviewProvider {
    static var previews: some View {
        DatabaseErrorView(status: .errorNotADatabase(dbFile: "simplex_v1_chat.db"))
    }
}
