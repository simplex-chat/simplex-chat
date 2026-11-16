//
//  VersionView.swift
//  SimpleXChat
//
//  Created by Evgeny on 22/01/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct VersionView: View {
    @State var versionInfo: CoreVersionInfo?

    var body: some View {
        VersionLayout(appVersion: appVersion, appBuild: appBuild, versionInfo: versionInfo)
            .onAppear {
                do {
                    versionInfo = try apiGetVersion()
                } catch let error {
                    logger.error("apiGetVersion error: \(responseError(error))")
                }
            }
    }
}

struct VersionLayout: View {
    @EnvironmentObject var theme: AppTheme
    let appVersion: String?
    let appBuild: String?
    let versionInfo: CoreVersionInfo?

    var body: some View {
        List {
            Section {
                Text("App version: v\(appVersion ?? "?")")
                Text("App build: \(appBuild ?? "?")")
                if let info = versionInfo {
                    Text("Core version: v\(info.version)")
                    if let v = try? AttributedString(markdown: "simplexmq: v\(info.simplexmqVersion) ([\(info.simplexmqCommit.prefix(7))](https://github.com/simplex-chat/simplexmq/commit/\(info.simplexmqCommit)))") {
                        Text(v)
                    }
                }
            }

            Section {
                NavigationLink {
                    DeveloperView()
                        .navigationTitle("Developer")
                        .modifier(ThemedBackground(grouped: true))
                } label: {
                    Text("Developer")
                }
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topLeading)
    }
}

struct VersionView_Previews: PreviewProvider {
    static var previews: some View {
        VersionLayout(appVersion: "7.1", appBuild: "363", versionInfo: CoreVersionInfo(version: "7.1.0.14", simplexmqVersion: "7.1.0.14", simplexmqCommit: "c0c416daf052eb0215ae432ae665522a7c96c0a6"))
    }
}
