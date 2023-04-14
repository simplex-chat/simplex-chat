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
        VStack(alignment: .leading) {
            Text("App version: v\(appVersion ?? "?")")
            Text("App build: \(appBuild ?? "?")")
            if let info = versionInfo {
                Text("Core version: v\(info.version)")
                if let v = try? AttributedString(markdown: "simplexmq: v\(info.simplexmqVersion) ([\(info.simplexmqCommit.prefix(7))](https://github.com/simplex-chat/simplexmq/commit/\(info.simplexmqCommit)))") {
                    Text(v)
                }
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topLeading)
        .padding()
        .onAppear {
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
        VersionView()
    }
}
