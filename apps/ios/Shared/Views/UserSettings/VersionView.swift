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
        Form {
            LabeledContent("App version", content: {
                Text("v\(appVersion ?? "?")")
            })
            LabeledContent("App build", content: {
                Text("\(appBuild ?? "?")")
            })
            if let info = versionInfo {
                LabeledContent("Core version", content: {
                    Text("v\(info.version)")
                })
                if let v = try? AttributedString(markdown: "v\(info.simplexmqVersion) ([\(info.simplexmqCommit.prefix(7))](https://github.com/simplex-chat/simplexmq/commit/\(info.simplexmqCommit)))") {
                    LabeledContent("SimpleX MQ", content: {
                        Text(v)
                    })
                }
            }
        }
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
