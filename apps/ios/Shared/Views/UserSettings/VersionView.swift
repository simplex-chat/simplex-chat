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
    @EnvironmentObject var theme: AppTheme
    @State var versionInfo: CoreVersionInfo?
    // do not pass
    var previewVersionInfo: CoreVersionInfo? = nil

    var body: some View {
        List {
            Section {
                Text("App version: v\(appVersion ?? "?")")
                Text("App build: \(appBuild ?? "?")")
                if let info = previewVersionInfo ?? versionInfo {
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
        .onAppear {
            if previewVersionInfo != nil { return }
            do {
                versionInfo = try apiGetVersion()
            } catch let error {
                logger.error("apiGetVersion error: \(responseError(error))")
            }
        }
    }
}

struct VersionView_Previews: PreviewProvider {
    static var previews: some View {
        VersionView(previewVersionInfo: CoreVersionInfo(version: "7.1.0.14", simplexmqVersion: "7.1.0.14", simplexmqCommit: "c0c416daf052eb0215ae432ae665522a7c96c0a6"))
    }
}
